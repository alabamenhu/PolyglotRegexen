#!/usr/bin/env raku
use v6.d;


sub MAIN(*@files, :$remove, :$add, :$*output where {$_ ~~ any <stdout rewrite trial>} = 'stdout', :$namespace) {
    die "Cannot specify add and remove, that doesn't make any sense" if $add && $remove;
    die "Need to have a namespace" unless $namespace;

    if $add {
        add $_, :$namespace for @files.map(*.IO)
    } else {
        remove $_, :$namespace for @files.map(*.IO)
    }
}

sub add(IO::Path $file, :$namespace) {
    # preparse, finding local token names
    my @local-tokens;
    for $file.lines -> $line {
        if $line ~~ /^ \h* [multi|proto]? \h* [token|regex|rule|method] \h* $<name>=(<[a..zA..Z-]>+) [\h | \: | \(] / {
            @local-tokens.push: $<name>.Str;
        }
    }

    @local-tokens .= unique;
    my $temp;

    # reparse, substituting as needed
    for $file.lines -> $line is copy {
        $line ~~ s:g/(token|regex|rule|method) \h* (@local-tokens)/$0 $namespace$1/;
        $line ~~ s:g/\<(@local-tokens)\>/\<$namespace$0\>/;
        $line ~~ s:g/self\.(@local-tokens)\>/self\.$namespace$0\>/;
        given $*output {
            when 'stdout'          { say $line             }
            when 'trial'|'rewrite' { $temp ~= $line ~ "\n" }
        }
    }
    if $*output eq 'trial' {
        $file.sibling($file.basename ~ '.namespaced').spurt: $temp
    }elsif $*output eq 'rewrite' {
        $file.spurt: $temp;
    }
}

sub remove(IO::Path $file, :$namespace) {
    my $temp;
    for $file.lines -> $line is copy {
        $line ~~ s:g/<?after ^ \h* [proto|multi]? \h* [token|regex|rule|method] \h* > $namespace //; # token declarations
        $line ~~ s:g/<?after \<> $namespace <?before <[a..zA..Z-]>+> //;                             # <token calls>
        $line ~~ s:g/<?after self\.> $namespace <?before <[a..zA..Z-]>+> //;                         # self.method calls
        given $*output {
            when 'stdout'          { say $line             }
            when 'trial'|'rewrite' { $temp ~= $line ~ "\n" }
        }
    }
    if $*output eq 'trial' {
        $file.sibling($file.basename ~ '.denamespaced').spurt: $temp
    }elsif $*output eq 'rewrite' {
        $file.spurt: $temp;
    }
}