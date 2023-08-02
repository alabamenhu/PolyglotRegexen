use v6.d;
unit module TestSupport;
use experimental :rakuast;
use Polyglot::Regex::ECMA262::Grammar;
use Polyglot::Regex::ECMA262::Actions;
use MONKEY-SEE-NO-EVAL;

grammar G does ECMA262-Regex {
    token TOP { <ECMA262-Regex-TOP> }
}

class A does ECMA262-Regex-Actions {
    method TOP ($/) { make $<ECMA262-Regex-TOP>.made }
}

#| Parses as an ECMA regex, turns into an evaluated Raku regex,
#| and then grabs the Raku code for reconstructing it.  Eventually,
#| this should use DEPARSE or similar once RakuAST regex nodes are
#| sufficiently capable.
sub ecma2raku(Str $ecma) is export {
    # The actual result is /{…}regex/, where the block applies a role.
    # That's a lot of extra crud to deal with, so take out the prefix and slashes
    my $s = G.parse($ecma, :actions(A)).made;

    $s = (EVAL RakuAST::QuotedRegex.new(body => $s)).gist;
    $s ~~ /:r # ratchet, token style
           ^\/ \h* \{ # prefix is currently just /{
           .*?     # match frugally until
           \}      # the end of the code block }
           <(.*)>  # then capture everything else
    /;
    '/' ~ $/.Str.subst(/\s+/,' ', :g)
}

sub ecma-match(Str $ecma, Str $text) is export {
    my $s = G.parse($ecma, :actions(A)).made;
    #say $s;
    $s = EVAL RakuAST::QuotedRegex.new(body => $s);
    #~ ($text ~~ $s)
}
