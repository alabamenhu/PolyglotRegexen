unit role ECMA262-Regex-Actions;
my sub lk(Mu \match, \key) { match.hash.AT-KEY: key }
my class Wildcard {
    method Str {
        $*ECMA262-DOT-MATCHES-NEWLINE ?? '.' !! '<-[\\n]>'
    }
}
my class StartAnchor {
    method Str {
        $*ECMA262-MULTILINE-MODE ?? '^^' !! '^'
    }
}
my class EndAnchor {
    method Str {
        $*ECMA262-MULTILINE-MODE ?? '$$' !! '$'
    }
}

my sub list-join(@x,+@y) { @x.head.Slip, (|@y, |$_ for @x.skip(1)).flat.Slip }
my multi sub infix:<,~> (\a, \b) { |a, |b }

my role FLIP-POLARITY { ; }
method ECMA262-Regex-TOP (Mu $/) {
    use MONKEY-SEE-NO-EVAL;

    use Polyglot::Regex::ECMA262::Role;
    make EVAL (
        ~ 'rx'
        ~ ($*ECMA262-CASE-INSENSITIVE ?? ':i' !! '')
        ~ "/\{use Polyglot::Regex::ECMA262::Role;\$/ does ECMA262-Regex-Match\}"
        ~ lk($/,'ECMA262-Regex-disjunction').made.join # join will stringify each element (most are raw strings already)
        ~ "/"
    );
}

method ECMA262-Regex-postoptions (Mu $/) {
    given $/.Str {
        when 'm' { $*ECMA262-DOT-MATCHES-NEWLINE = True }
        when 's' { $*ECMA262-MULTILINE-MODE      = True }
        when 'i' { $*ECMA262-CASE-INSENSITIVE    = True }
    }
    $*ECMA262-HAD-POSTOPTIONS = True;
}

method ECMA262-Regex-disjunction (Mu $/) {
    make lk($/,'ECMA262-Regex-alternative') == 1
        ?? lk($/,'ECMA262-Regex-alternative').head.made
        !! lk($/,'ECMA262-Regex-alternative').map(*.made).&list-join('||')
}
method ECMA262-Regex-alternative (Mu $/) {
    make lk($/,'ECMA262-Regex-term') == 1
        ?? lk($/,'ECMA262-Regex-term').head.made
        !! lk($/,'ECMA262-Regex-term').map(*.made).&list-join # space not needed, but makes debugging easier
}
method ECMA262-Regex-term (Mu $/) {
    with lk($/,'ECMA262-Regex-assertion') {
        make lk($/,'ECMA262-Regex-assertion').made;
    } else {
        make lk($/,'ECMA262-Regex-atom').made ,~ (.made with lk($/,'ECMA262-Regex-quantifier'))
    }
}

method ECMA262-Regex-quantifier (Mu $/) {
    my $frugal = lk($/,'frugal').Str;
    if    lk($/,'ECMA262-Regex-quantifier-prefix').Str eq '?' { make "?$frugal" }
    elsif lk($/,'ECMA262-Regex-quantifier-prefix').Str eq '+' { make "+$frugal" }
    elsif lk($/,'ECMA262-Regex-quantifier-prefix').Str eq '*' { make "*$frugal" }
    else {
        with lk(lk($/,'ECMA262-Regex-quantifier-prefix'),'max') {
            make " **$frugal " ,~ lk(lk($/,'ECMA262-Regex-quantifier-prefix'),'min').Str ,~ '..' ,~ lk(lk($/,'ECMA262-Regex-quantifier-prefix'),'max') ,~ ' '
        } else {
            with lk(lk($/,'ECMA262-Regex-quantifier-prefix'),'range') {
                make " **$frugal " ,~ lk(lk($/,'ECMA262-Regex-quantifier-prefix'),'min').Str ,~ '..* '
            }else {
                make " **$frugal " ,~ lk(lk($/,'ECMA262-Regex-quantifier-prefix'),'min').Str ,~ ' '
            }
        }
    }
}

proto method ECMA262-Regex-atom { * }
method ECMA262-Regex-atom:literal (Mu $/) {
    # make RakuAST::Regex::Literal.new(Mu $/.Str)
    my $letter = $/.Str;
    my $type   = $letter.uniprop;

    if    $type.starts-with('L') { make        $letter       }
    elsif $type    eq 'Nd'       { make        $letter       }
    elsif $letter  eq  '_'       { make        $letter       }
    elsif $type.starts-with('Z') { make "'"  ~ $letter ~ "'" }
    else                         { make "\\" ~ $letter       }
}

method ECMA262-Regex-atom:any              (Mu $/) { make Wildcard }
method ECMA262-Regex-atom:escape           (Mu $/) { make lk($/,'ECMA262-Regex-atom-escape').made     }
method ECMA262-Regex-atom:char-class       (Mu $/) { make lk($/,'ECMA262-Regex-character-class').made }
method ECMA262-Regex-atom:noncapture-group (Mu $/) { make '[' ,~ lk($/,'ECMA262-Regex-disjunction').made ,~ ']' }
method ECMA262-Regex-atom:capture-group    (Mu $/) {
    with lk($/,'ECMA262-Regex-group-specifier') {
        make '$<' ,~ .made ,~ '>=(' ,~ lk($/,'ECMA262-Regex-disjunction').made ,~ ')'
    }else{
        make '(' ,~ lk($/,'ECMA262-Regex-disjunction').made ,~ ')'
    }
}

method ECMA262-Regex-group-specifier (Mu $/) {
    make lk($/,'ECMA262-Regex-group-name').made
}
method ECMA262-Regex-group-name (Mu $/) {
    make lk($/,'ECMA262-Regex-regex-identifier-name').made
}

method ECMA262-Regex-regex-identifier-name (Mu $/) {
    make lk($/,'ECMA262-Regex-identifier-start-char').made ,~ (.map(*.made).join with lk($/,'CMA262-Regex-identifier-part-char'))
}

method ECMA262-Regex-identifier-start-char (Mu $/) {
    with lk($/,'ECMA262-Regex-unicode-escape-sequence') { make lk($/,'ECMA262-Regex-unicode-escape-sequence').made}
    else { make $/.Str }
}
method ECMA262-Regex-identifier-part-char (Mu $/) {
    with lk($/,'ECMA262-Regex-unicode-escape-sequence') { make lk($/,'ECMA262-Regex-unicode-escape-sequence').made}
    else { make $/.Str }
}


my %character-class-escape is Map =
    d => '<[0..9]>',
    D => '<-[0..9]>',
    w => '<[a..zA..Z_-]>',
    W => '<-[a..zA..Z_-]>',
    w => '<[\\x9\\xb\\xc\\xFEFF]+:Zs]>',
    W => '<-[\\x9\\xb\\xc\\xFEFF]-:Zs]>';

method ECMA262-Regex-atom-escape:char-class-escape (Mu $/) {
    make lk(%character-class-escape,$/.Str);
}
method ECMA262-Regex-atom-escape:char-escape (Mu $/) {
    make lk($/,'ECMA262-Regex-character-escape').made
}
method ECMA262-Regex-character-escape (Mu $/) {
    make .made with lk($/, 'ECMA262-Regex-control-escape');
    make .made with lk($/, 'ECMA262-Regex-control-letter');
    make .made with lk($/, 'ECMA262-Regex-hex-escape-sequence');
    make .made with lk($/, 'ECMA262-Regex-unicode-escape-sequence');
    make .made with lk($/, 'ECMA262-Regex-identity-escape');
    make .made with lk($/, 'ECMA262-Regex-unicode-property');
}
method ECMA262-Regex-character-escape-in-char-class (Mu $/) {
    make .made with lk($/, 'ECMA262-Regex-control-escape-in-char-class');
    make .made with lk($/, 'ECMA262-Regex-control-letter-in-char-class');
    make .made with lk($/, 'ECMA262-Regex-hex-escape-sequence-in-char-class');
    make .made with lk($/, 'ECMA262-Regex-unicode-escape-sequence-in-char-class');
    make .made with lk($/, 'ECMA262-Regex-identity-escape-in-char-class');
    make .made with lk($/, 'ECMA262-Regex-unicode-property-in-char-class');
}

my %control-escape is Map =
    'f', "\\xC ", # post space to avoid hex clash
    'n', "\\xA ",
    'r', "\\xD ",
    't', "\\x9 ",
    'v', "\\xB "
;
method ECMA262-Regex-control-escape          (Mu $/) { make %control-escape.AT-KEY($/.Str)          }
method ECMA262-Regex-control-letter          (Mu $/) { make '\\x' ~ ($/.lc.ord - 96).base(16) ~ ' ' }
method ECMA262-Regex-hex-escape-sequence     (Mu $/) { make '\\x' ~ $/.Str ~ ' '                    }
method ECMA262-Regex-unicode-escape-sequence (Mu $/) { make '\\x' ~ $/.Str ~ ' '                    }
method ECMA262-Regex-identity-escape         (Mu $/) { make '\\'  ~ $/.Str                          }
method ECMA262-Regex-unicode-property        (Mu $/) {

    say "did unicode property!";
    say "---> ", lk($/,'ECMA262-Regex-unicode-property-value-expression').made;
    make (lk($/,'type') eq 'p' ?? '<:' !! '<:!') ,~  lk($/,'ECMA262-Regex-unicode-property-value-expression').made ,~ '>'
}

method ECMA262-Regex-control-escape-in-char-class          (Mu $/) { make %control-escape.AT-KEY($/.Str)          }
method ECMA262-Regex-control-letter-in-char-class          (Mu $/) { make '\\x' ~ ($/.lc.ord - 96).base(16) ~ ' ' }
method ECMA262-Regex-hex-escape-sequence-in-char-class     (Mu $/) { make '\\x' ~ $/.Str ~ ' '                    }
method ECMA262-Regex-unicode-escape-sequence-in-char-class (Mu $/) { make '\\x' ~ $/.Str ~ ' '                    }
method ECMA262-Regex-identity-escape-in-char-class         (Mu $/) { make '\\'  ~ $/.Str                          }
method ECMA262-Regex-unicode-property-in-char-class        (Mu $/) {
    make (lk($/,'type') eq 'p' ?? ':' !! ':!') ,~  lk($/,'ECMA262-Regex-unicode-property-value-expression').made ,~ ' '
}

proto method ECMA262-Regex-assertion { * }
method ECMA262-Regex-assertion:str-start (Mu $/)        { make StartAnchor } # '^'
method ECMA262-Regex-assertion:str-end (Mu $/)          { make EndAnchor   } # '$'
method ECMA262-Regex-assertion:start (Mu $/)            { make StartAnchor } # '^'
method ECMA262-Regex-assertion:word-boundary (Mu $/)    { make '<wb>'  }
method ECMA262-Regex-assertion:neg-word-boundary (Mu $/){ make '<!wb>' }
method ECMA262-Regex-assertion:pos-lookahead (Mu $/)    { make '<?before ' ,~ lk($/,'ECMA262-Regex-disjunction').made ,~ '>' }
method ECMA262-Regex-assertion:neg-lookahead (Mu $/)    { make '<!before ' ,~ lk($/,'ECMA262-Regex-disjunction').made ,~ '>' }
method ECMA262-Regex-assertion:pos-lookback (Mu $/)     { make '<?after '  ,~ lk($/,'ECMA262-Regex-disjunction').made ,~ '>' }
method ECMA262-Regex-assertion:neg-lookback (Mu $/)     { make '<!after '  ,~ lk($/,'ECMA262-Regex-disjunction').made ,~ '>' }

method ECMA262-Regex-character-class (Mu $/) {
    my $inner = lk($/,'ECMA262-Regex-class-ranges').made;
    make $inner ?? ('<' ,~ $inner ,~ '>') !! ''
}

method ECMA262-Regex-class-ranges (Mu $/) {
    with lk($/,'ECMA262-Regex-non-empty-class-ranges') {
        make .made
    }else {
        make ''
    }
}

my proto sub polarity(|) {*}
multi sub polarity(|)  { $*NEGATED-CHAR-CLASS ?? '-' !! '+' }
multi sub polarity(FLIP-POLARITY) { $*NEGATED-CHAR-CLASS ?? '+' !! '-' }

# Adding brackets is potentially overkill, but this lets us do things in a single pass
method ECMA262-Regex-non-empty-class-ranges:range (Mu $/) {
    make polarity() ,~ '[' ,~ lk($/,'ECMA262-Regex-class-atom').head.made ,~ '..' ,~ lk($/,'ECMA262-Regex-class-atom').tail.made ,~ ']' ,~ (.made with lk($/,'ECMA262-Regex-class-ranges')); # <-- the subsequent ones
}
method ECMA262-Regex-non-empty-class-ranges:no-dash (Mu $/) {
    my $made = lk($/,'ECMA262-Regex-class-atom-no-dash').made;
    make $made.head.starts-with(':') # is it a unicode class?
        ?? polarity($made) ,~       $made         ,~ (lk($/,'ECMA262-Regex-non-empty-class-ranges-no-dash').?made // '')
        !! polarity($made) ,~ '[' ,~ $made ,~ ']' ,~ (lk($/,'ECMA262-Regex-non-empty-class-ranges-no-dash').?made // '')
}
method ECMA262-Regex-non-empty-class-ranges:dashable (Mu $/) {
    my $made = lk($/,'ECMA262-Regex-class-atom').made;
    make $made.head.starts-with(':') # is it a unicode class?
        ?? polarity($made) ,~        $made
        !! polarity($made) ,~ '[' ,~ $made ,~ ']'
}

# Basically the same
method ECMA262-Regex-non-empty-class-ranges-no-dash:range (Mu $/) {
        make polarity() ,~ '[' ,~ lk($/,'ECMA262-Regex-class-atom-no-dash').made ,~ '..' ,~ lk($/,'ECMA262-Regex-class-atom').made ,~ ']' ,~ (.made with lk($/,'ECMA262-Regex-class-ranges')); # <-- the subsequent ones
}
method ECMA262-Regex-non-empty-class-ranges-no-dash:no-dash (Mu $/) {
    my $made = lk($/,'ECMA262-Regex-class-atom-no-dash').made;
    make $made.head.starts-with(':') # is it a unicode class?
        ?? polarity($made) ,~       $made         ,~ (lk($/,'ECMA262-Regex-non-empty-class-ranges-no-dash').?made // '')# <-- the subsequent ones
        !! polarity($made) ,~ '[' ,~ $made ,~ ']' ,~ (lk($/,'ECMA262-Regex-non-empty-class-ranges-no-dash').?made // '')# <-- the subsequent ones
}
method ECMA262-Regex-non-empty-class-ranges-no-dash:dashable (Mu $/) {
    my $made = lk($/,'ECMA262-Regex-class-atom').made;
    make $made.head.starts-with(':') # is it a unicode class?
        ?? polarity($made) ,~       $made
        !! polarity($made) ,~ '[' ,~ $made ,~ ']'
}

method ECMA262-Regex-class-atom(Mu $/) {
    if   $/.Str eq '-' { make '\-'                                           }
    else               { make lk($/,'ECMA262-Regex-class-atom-no-dash').made }
}

method ECMA262-Regex-class-atom-no-dash (Mu $/) {
    my $char = $/.Str;
    if $char.starts-with('\\') { # is escaped in some way
        make lk($/,'ECMA262-Regex-class-escape').made
    }
    else { # normal character
        my $prop = $char.uniprop;
        if $prop.starts-with('L') || $prop eq 'Nd' || $char eq '_' {
            make $char;
        } else {
            make '\\' ~ $char
        }
    }
}

method ECMA262-Regex-class-escape:backreference (Mu $/) {
    make '<{ $¢[' ,~ $/.Str ,~ '] }>'
}
method ECMA262-Regex-class-escape:backspace (Mu $/) {
    make '\\b'
}

# This method is perhaps confusing, and could probably be done more elegantly down the road.
# Each class is boxed in ±[…] higher up, but the ± has to be applied to each […] individually.
# Since the space one ends with a unicode class, no bracket is necessary, so we sandwich it
# to guarantee correct regex structure when they are added.
method ECMA262-Regex-class-escape:char-class (Mu $/) {
    my constant %escape =
        dTrue => '0..9',
        DTrue => '0..9',
        dFalse => '0..9',
        DFalse => '0..9',
        wTrue => 'a..zA..Z_-',
        WTrue => 'a..zA..Z_-',
        wFalse => 'a..zA..Z_-',
        WFalse => 'a..zA..Z_-',
        sTrue => '\\x9\\xb\\xc]-:Zs-[\\xFEFF',
        STrue => '\\x9\\xb\\xc]+:Zs+[\\xFEFF',
        sFalse => '\\x9\\xb\\xc]+:Zs+[\\xFEFF',
        SFalse => '\\x9\\xb\\xc]-:Zs-[\\xFEFF';
    $/.Str eq 'D' | 'W' | 'S'
        ?? make lk(%escape,$/ ,~ $*NEGATED-CHAR-CLASS) but FLIP-POLARITY
        !! make lk(%escape,$/ ,~ $*NEGATED-CHAR-CLASS)
}
method ECMA262-Regex-class-escape:char-escape (Mu $/) {
    make lk($/, 'ECMA262-Regex-character-escape-in-char-class').made;
}

method ECMA262-Regex-atom:external-regex (Mu $/) {
    make '<' ,~ $/.Str ,~ '>'
}


method ECMA262-Regex-unicode-property-value-expression:value (Mu $/) {
    make $/.Str
}
method ECMA262-Regex-unicode-property-value-expression:key-value (Mu $/) {
    make lk($/,'ECMA262-Regex-unicode-property-name').Str ,~ '<' ,~ lk($/,'ECMA262-Regex-unicode-property-value').Str ,~ '>'
}