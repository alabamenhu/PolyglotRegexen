use v6.d;
unit role ECMA262-Regex;

##############################################################
## Code is written so that it can be name-space transformed ##
##############################################################

#| The baseline definition of a regex.  Does not include typical // delineators
#| but via two dynamic variables can optionally search for a terminal character
#| other than $ (for instance, /) and can optionally allow trailing whitespace
#| up to that terminal character ($*ECMA262-ENDER and $*ECMA262-SPACE-FLANKED)
token ECMA262-Regex-TOP {
    # These regex options need to be set this way in case another regex intends to
    # incorporate the TOP token and wants to pass it (as the prime example, one
    # need only look at the Grammar-Mixin for Polyglot::Regex::ECMA262.  Normally,
    # you'd only need CALLERS::<$*foo>, but because that ends up referring to the token
    # (which in turn calls that codeblock, and thus there's a Nil default value from
    # that scope), we use an extra CALLER:: to bump us one frame level up before
    # looking in the stash.
    :my $*ECMA262-CASE-INSENSITIVE;    { $*ECMA262-CASE-INSENSITIVE    = CALLER::CALLERS::<$*ECMA262-CASE-INSENSITIVE>    // False }
    :my $*ECMA262-MULTILINE-MODE;      { $*ECMA262-MULTILINE-MODE      = CALLER::CALLERS::<$*ECMA262-MULTILINE-MODE>      // False }
    :my $*ECMA262-DOT-MATCHES-NEWLINE; { $*ECMA262-DOT-MATCHES-NEWLINE = CALLER::CALLERS::<$*ECMA262-DOT-MATCHES-NEWLINE> // False }
    :my $*ECMA262-GROUP-COUNTER;       { $*ECMA262-GROUP-COUNTER       = 1}
    :my %*ECMA262-GROUP-NAMES;

    <ECMA262-Regex-disjunction>
    [
        <.ECMA262-Regex-allow-postoptions>
        <.ECMA262-Regex-check-terminator>
        .
        <ECMA262-Regex-postoptions>
    ]?
    <?before
        [<ECMA262-Regex-space-flanking> \s*! ]?
        [
        || <?{$*ECMA262-HAD-POSTOPTIONS}>
        || <ECMA262-Regex-check-terminator>
        || $
        ]
    >
}

token ECMA262-Regex-postoptions {
    [
    | s                                                                        # 'Dot for newline'
    | m                                                                        # 'Multiline'
    | g <.panic("Modifier '{$/.Str}' not yet supported or not possible here")> # 'Global'
    | d <.panic("Modifier '{$/.Str}' not yet supported or not possible here")> # 'Substring match indices'
    | i                                                                        # 'Case insensitive'
    | u <.panic("Modifier '{$/.Str}' not yet supported or not possible here")> # 'Unicode'
    | y <.panic("Modifier '{$/.Str}' not yet supported or not possible here")> # 'Sticky'
    ]+
    { $*ECMA262-HAD-POSTOPTIONS = True }
}

#| Alternatives in a regex (may only be one).  In ECMA262, these are
#| *always* checked in LTR order.
token ECMA262-Regex-disjunction { <ECMA262-Regex-alternative>* % '|' }

#| A single sequence of terms choice that could be included in a disjunction.
token ECMA262-Regex-alternative { <ECMA262-Regex-term>* }

#| Always succeeds token (needed because of restrictions in NQPRegex)
token ECMA262-Regex-Succeed { <?> }

#| Always fails token (needed because of restrictions in NQPRegex)
token ECMA262-Regex-Fail { <!> }

#| Effectively does a lookahead for $*ECMA262-ENDER.  Variable interpolation
#| is not available in NQPRegex, so this support method is needed
method ECMA262-Regex-check-terminator {
    my $next = self.orig.substr(self.pos,1);
    with $*ECMA262-ENDER {
        return $next eq $*ECMA262-ENDER
            ?? self.ECMA262-Regex-Succeed
            !! self.ECMA262-Regex-Fail
    }
    return self.ECMA262-Regex-Fail
}

#| Asserts via $*ECMA262-SPACE-FLANKED whether we allow the regex to have trailing spaces that are ignored
method ECMA262-Regex-space-flanking {
    $*ECMA262-SPACE-FLANKED
        ?? self.ECMA262-Regex-Succeed
        !! self.ECMA262-Regex-Fail
}

#| Asserts via $*ECMA262-SPACE-FLANKED whether we allow the regex to have trailing spaces that are ignored
method ECMA262-Regex-allow-postoptions {
    $*ECMA262-ALLOW-POSTOPTIONS
        ?? self.ECMA262-Regex-Succeed
        !! self.ECMA262-Regex-Fail
}

#| Either a single character or a single construct such as a grou
token ECMA262-Regex-term {
    # First ensure we shouldn't be finished (e.g. by reaching a terminal character)
    <!before [<.ECMA262-Regex-space-flanking> \s*!]? [$ | <.ECMA262-Regex-check-terminator>]>
    [
        || <ECMA262-Regex-assertion>                        # construct such as grouping (note! repetition not allowed!)
        || <ECMA262-Regex-atom> <ECMA262-Regex-quantifier>? # single character, possibly repeated
        || <!>                                              # fail automatically if we reach here somehow
    ]
}

#| A term that's not a single literal character (not quantifiable)
proto token ECMA262-Regex-assertion { * }
token ECMA262-Regex-assertion:str-start         { '^'   }
token ECMA262-Regex-assertion:str-end           { '$'   }
token ECMA262-Regex-assertion:start             { '^'   }
token ECMA262-Regex-assertion:word-boundary     { '\\b' }
token ECMA262-Regex-assertion:neg-word-boundary { '\\B' }
token ECMA262-Regex-assertion:pos-lookahead     { '(?='  <ECMA262-Regex-disjunction> ')' }
token ECMA262-Regex-assertion:neg-lookahead     { '(?!'  <ECMA262-Regex-disjunction> ')' }
token ECMA262-Regex-assertion:pos-lookback      { '(?<=' <ECMA262-Regex-disjunction> ')' }
token ECMA262-Regex-assertion:neg-lookback      { '(?<!' <ECMA262-Regex-disjunction> ')' }

#| Dictates how many times an atom should be repeated
token ECMA262-Regex-quantifier { <ECMA262-Regex-quantifier-prefix> $<frugal>='?'? }
token ECMA262-Regex-quantifier-prefix {
    | '+' | '*' | '?'
    | '{' $<min>=<.ECMA262-Regex-decimal-digits> [ $<range>=',' [$<max>=<.ECMA262-Regex-decimal-digits>]? ]? '}'
}

#| The core of matching: this part of the regex reflects actual text that can be matched
proto token ECMA262-Regex-atom { * }
token ECMA262-Regex-atom:literal          { <-[^$\\.*+?()[\]{}|]> }
token ECMA262-Regex-atom:any              { \. } # TODO: handle /s flag?
token ECMA262-Regex-atom:escape           { \\ <ECMA262-Regex-atom-escape> }
token ECMA262-Regex-atom:char-class       { <ECMA262-Regex-character-class> }
token ECMA262-Regex-atom:capture-group    {
    \(   <ECMA262-Regex-group-specifier>?
    :my $*ECMA262-GROUP-COUNT = $*ECMA262-GROUP-COUNTER++;
    <ECMA262-Regex-disjunction> \)
}
token ECMA262-Regex-atom:noncapture-group { \(\?\:                    <ECMA262-Regex-disjunction> \) }
token ECMA262-Regex-atom:external-regex   { \(\?\.\< <( <.identifier> )> \>\) }

#| An escaped character inside of a regex (e.g. /\s/, /\x42/, or \7)
proto token ECMA262-Regex-atom-escape { * }
token ECMA262-Regex-atom-escape:decimal           { <.ECMA262-Regex-decimal-digits>         }
token ECMA262-Regex-atom-escape:named             { k  <ECMA262-Regex-group-name>           }
token ECMA262-Regex-atom-escape:char-escape       { <ECMA262-Regex-character-escape>        }
token ECMA262-Regex-atom-escape:char-class-escape { <.ECMA262-Regex-character-class-escape> }

## These escapes are used OUTSIDE of (enumerated) character classes
token ECMA262-Regex-character-escape {
    | <ECMA262-Regex-control-escape>
    | 'c' <ECMA262-Regex-control-letter>
    | <ECMA262-Regex-hex-escape-sequence>
    | <ECMA262-Regex-unicode-escape-sequence>
    | <ECMA262-Regex-identity-escape>
    | <ECMA262-Regex-unicode-property>
}
token ECMA262-Regex-control-escape          {        <[fnrtv]>              }
token ECMA262-Regex-control-letter          {        <[A..Za..z]>           }
token ECMA262-Regex-hex-escape-sequence     { 'x' <( <[0..9A..Fa..f]>**2 )> }
token ECMA262-Regex-unicode-escape-sequence { 'u' <( <[0..9A..Fa..f]>**4 )> }
token ECMA262-Regex-identity-escape         { <-ident-[\c[ZWJ]\c[ZWNJ]]>    }
token ECMA262-Regex-unicode-property        { $<type>=<[pP]> \{ <ECMA262-Regex-unicode-property-value-expression> \} }


#| Simple latin decimal digits in sequence. (Only used for backreferences in ECMA262)
token ECMA262-Regex-decimal-digits { <[0..9]>+ }

# per ECMA, [a-b] works, but [\w-x] does NOT
# we specify valid parts for char class by passing a variable
token ECMA262-Regex-character-class-escape { <[dDsSwW]> }
token ECMA262-Regex-character-class {
    '['
        $<negated>='^'?
        <ECMA262-Regex-class-ranges>
    ']'
}

# These are the same escapes, except used INSIDE of (enumerated) character classes
token ECMA262-Regex-character-escape-in-char-class {
    |     <ECMA262-Regex-control-escape-in-char-class>
    | 'c' <ECMA262-Regex-control-letter-in-char-class>
    |     <ECMA262-Regex-hex-escape-sequence-in-char-class>
    |     <ECMA262-Regex-unicode-escape-sequence-in-char-class>
    |     <ECMA262-Regex-identity-escape-in-char-class>
    |     <ECMA262-Regex-unicode-property-in-char-class>
}
token ECMA262-Regex-control-escape-in-char-class          {        <[fnrtv]>              }
token ECMA262-Regex-control-letter-in-char-class          {        <[A..Za..z]>           }
token ECMA262-Regex-hex-escape-sequence-in-char-class     { 'x' <( <[0..9A..Fa..f]>**2 )> }
token ECMA262-Regex-unicode-escape-sequence-in-char-class { 'u' <( <[0..9A..Fa..f]>**4 )> }
token ECMA262-Regex-identity-escape-in-char-class         { <-ident-[\c[ZWJ]\c[ZWNJ]]>    }
token ECMA262-Regex-unicode-property-in-char-class        { $<type>=<[pP]> \{ <ECMA262-Regex-unicode-property-value-expression> \} }

token ECMA262-Regex-class-ranges {
    <ECMA262-Regex-non-empty-class-ranges>?
}

proto
token ECMA262-Regex-non-empty-class-ranges { * }
token ECMA262-Regex-non-empty-class-ranges:range    { :my $*ALLOW-CHAR-CLASS := False; <ECMA262-Regex-class-atom> '-' <ECMA262-Regex-class-atom> <ECMA262-Regex-class-ranges> }
token ECMA262-Regex-non-empty-class-ranges:no-dash  { :my $*ALLOW-CHAR-CLASS := True;  <ECMA262-Regex-class-atom-no-dash> <ECMA262-Regex-non-empty-class-ranges-no-dash>? }
token ECMA262-Regex-non-empty-class-ranges:dashable { :my $*ALLOW-CHAR-CLASS := True;  <ECMA262-Regex-class-atom> <ECMA262-Regex-non-empty-class-ranges-no-dash>? } #javascript allows this, the standard disagrees

proto
token ECMA262-Regex-non-empty-class-ranges-no-dash { * }
token ECMA262-Regex-non-empty-class-ranges-no-dash:range    { :my $*ALLOW-CHAR-CLASS := False; <ECMA262-Regex-class-atom-no-dash> '-' <ECMA262-Regex-class-atom> <ECMA262-Regex-class-ranges> }
token ECMA262-Regex-non-empty-class-ranges-no-dash:no-dash  { :my $*ALLOW-CHAR-CLASS := True;  <ECMA262-Regex-class-atom-no-dash> <ECMA262-Regex-non-empty-class-ranges-no-dash> }
token ECMA262-Regex-non-empty-class-ranges-no-dash:dashable { :my $*ALLOW-CHAR-CLASS := True;  <ECMA262-Regex-class-atom> }

token ECMA262-Regex-class-atom {
    | $<dash>='-'
    | <ECMA262-Regex-class-atom-no-dash>
}
token ECMA262-Regex-class-atom-no-dash {
    | $<single>=<-[\\\]-]>
    | \\ <ECMA262-Regex-class-escape>
}

proto token ECMA262-Regex-class-escape (|) { * }
token ECMA262-Regex-class-escape:backreference { <ECMA262-Regex-decimal-digits>         }
token ECMA262-Regex-class-escape:backspace     { 'b'                      }
token ECMA262-Regex-class-escape:char-escape   { <ECMA262-Regex-character-escape-in-char-class>       }
token ECMA262-Regex-class-escape:char-class    { <?{$*ALLOW-CHAR-CLASS}> <ECMA262-Regex-character-class-escape> }
token ECMA262-Regex-group-specifier {
    '?' <ECMA262-Regex-group-name>
}
token ECMA262-Regex-group-name {
    '<' ~ '>' <ECMA262-Regex-regex-identifier-name>
}
token ECMA262-Regex-regex-identifier-name {
    <ECMA262-Regex-identifier-start-char>
    <ECMA262-Regex-identifier-part-char> *
}
token ECMA262-Regex-identifier-start-char {
    | <:ID_Start+[$_]>
    | '\\' <ECMA262-Regex-unicode-escape-sequence>
}
token ECMA262-Regex-identifier-part-char {
    | <:ID_Continue+[$\x200c\x200d]>
    | <:ID_Continue+[$\x200c\x200d]>
    | '\\' <ECMA262-Regex-unicode-escape-sequence>
}

proto token ECMA262-Regex-unicode-property-value-expression { * }
token ECMA262-Regex-unicode-property-value-expression:value     { <ECMA262-Regex-unicode-property-value> <!before '='> }
token ECMA262-Regex-unicode-property-value-expression:key-value { <ECMA262-Regex-unicode-property-name> '=' <ECMA262-Regex-unicode-property-value> }
token ECMA262-Regex-unicode-property-name  { <[_a..zA..Z]>+     }
token ECMA262-Regex-unicode-property-value { <[_a..zA..Z0..9]>+ }