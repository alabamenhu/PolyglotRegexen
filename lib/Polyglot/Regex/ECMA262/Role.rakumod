unit role ECMA262-Regex-Match is export;

# ECMA allows for three types of matches:
#   - Non-capture: ignore
#   - Normal: interpret as positional, indexed from left to right, irrespective of hierarchy
#   - Named: interpret as positional as normally, but also associate a name to the index.
# Thus in a regex such as
#    / a(?:b(c))(?<D>d)(e)/
# The correct interpretation is
#    0 abcde
#    1 c
#    2 d (and named as 'D')
#    3 e
# To avoid trying to overly complicate things, the approach here is the simplest.
# It may or may not be the most efficient, but it seems to have little overhead.
# At the start of an ECMA regex, there will be two calls inserted in a code block:
#    $¢ does ECMA-Regex-Match;
#    $¢.register-names: :2foo, :5bar;
# The first applies the role, the second sets the expected positional index for
# a named capture.  When a key is requested, this index is used as to find the
# correct match.
# Note that in ECMA, bad/unknown matches are undefined and in Raku they are Nil.

has @!matches is default(Nil) = self; # 0 = self
has %!names;

#| Registers the result of a match
method register-position($match is raw, Int() $position) {
    @!matches[$position] = $match;
}

#| Registers the names to be used in a match
method register-names(*@items) { %!names := Map.new(@items) }

method list { @!matches.List }
method hash { Map.new(%!names.map: {.key => @!matches[.value]} ) }

multi method AT-POS(Int \pos) {
    @!matches[pos]
}
multi method AT-KEY(Str \key) {
    return @!matches[$_] with %!names{key};
    Nil
}


method gist {
    my @matches;

    for self.list Z ^self.list -> ($match, $index) {
        @matches.push: %(:$match, key => $index, from => $match.from, to => $match.to),
    }

    @matches = @matches.sort: { .<from>, -.<to> };
    my %reverse = %!names.invert;
    my $gist = '｢' ~ self.Str ~ '｣ ᴇᴄᴍᴀ';
    $gist ~= "\n" ~ ' ' ~ (.key+1) ~ ('<' ~ $_ ~ '>' with %reverse{.key+1}) ~ ' => ｢' ~ .value.?Str ~ '｣'
        for @!matches[1..*].pairs;
    $gist
}

