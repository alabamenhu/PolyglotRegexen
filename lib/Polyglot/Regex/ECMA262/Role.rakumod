unit role ECMA262-Regex-Match is export;

#| Finds all matches of a given type ('positional' | 'named')
#| via a depth-first search in the match.  All matches that aren't
#| of type ECMA-Regex will have such a role applied.
my sub find-matches-in($match, :$type) {
    $match does ECMA262-Regex-Match unless $match ~~ ECMA262-Regex-Match;
    for $match.Match::chunks -> (:$key, :$value) {
        next if $key eq '~';
        my $numeric = so $key ~~ /^<[0..9]>+$/;
        take $value if $type eq 'positional' && $numeric;
        take ($key => $value) if $type eq 'named' && !$numeric;
        find-matches-in $value, :$type
    }
}
has Positional $!positional-cache;
has Map  $!named-cache;
method list { .return with $!positional-cache; $!positional-cache := @(eager gather take self and find-matches-in self, :type<positional>) }
method hash { .return with $!named-cache; $!named-cache = Map.new: eager gather               find-matches-in self, :type<named>      }

multi method AT-POS(Int \pos) { say "called [pos = {pos} with list ", self.list.map(*.Str); self.list[pos] }
multi method AT-KEY(Str \key) { self.hash{key} }


method gist {
    my @matches;

    for self.list Z ^self.list -> ($match, $index) {
        @matches.push: %(:$match, key => $index, from => $match.from, to => $match.to),
    }
    for self.hash.kv -> $key, $match {
        @matches.push: %(:$match, :$key, from => $match.from, to => $match.to),
    }

    @matches = @matches.sort: {.<from>, -.<to>};

    my $gist = '｢' ~ self.Str ~ '｣ ᴇᴄᴍᴀ';
    $gist ~= "\n" ~ ' ' ~ .<key> ~ ' => ｢' ~ .<match>.Str ~ '｣'
        for @matches;
    $gist
}

