unit role ECMA262-Regex-Actions;
use Polyglot::Regex::ECMA262::Classes;
use experimental :rakuast;

my sub lk(Mu \match, \key) { match.hash.AT-KEY: key }

my sub list-join(@x,+@y) { @x.head.Slip, (|@y, |$_ for @x.skip(1)).flat.Slip }
my multi sub infix:<,~> (\a, \b) { |a, |b }

my role FLIP-POLARITY { ; }

sub rx-block-blockoid-stmtlist (+@list) {
    RakuAST::Regex::Block.new(RakuAST::Block.new(
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new: |@list)
    ))
}
method ECMA262-Regex-TOP (Mu $/) {
    use MONKEY-SEE-NO-EVAL;

    use Polyglot::Regex::ECMA262::Role:auth<zef:guifa>;
    my $*ECMA262-FIRST-BACKREFERENCE = True;

    my $use-stmt = RakuAST::Statement::Use.new(
        module-name => RakuAST::Name.from-identifier-parts( | <Polyglot Regex ECMA262 Role>),
    );

    my $does-stmt = RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
            left => RakuAST::Var::Lexical.new('$¢'),
            infix => RakuAST::Infix.new('does'),
            right => RakuAST::Type::Simple.new(
                RakuAST::Name.from-identifier('ECMA262-Regex-Match')
            )
        )
    );
    my $reg-name-stmt = RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyPostfix.new(
            operand => RakuAST::Var::Lexical.new('$¢'),
            postfix => RakuAST::Call::Method.new(
                name => RakuAST::Name.from-identifier('register-names'),
                |((args => RakuAST::ArgList.new(
                    %*ECMA262-GROUP-NAMES.map:
                        {RakuAST::ColonPair::Number.new(key => .key, value => .value)}
                )) if %*ECMA262-GROUP-NAMES)
            )
        )
    );

    my @main-stmts = lk($/,'ECMA262-Regex-disjunction').made.map(*.GENERATE);
    # options ($*ECMA262-CASE-INSENSITIVE ?? ':i' !! '')
    make RakuAST::Regex::Sequence.new(
        rx-block-blockoid-stmtlist(
            $use-stmt,
            $does-stmt,
            $reg-name-stmt,
        ),
        |@main-stmts
    )
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
    make Disjunction.new: lk($/,'ECMA262-Regex-alternative').map(*.made)
}
method ECMA262-Regex-alternative (Mu $/) {
    make Alternative.new: lk($/,'ECMA262-Regex-term').map(*.made)
}

method ECMA262-Regex-term (Mu $/) {
    with lk($/,'ECMA262-Regex-assertion') {
        make lk($/,'ECMA262-Regex-assertion').made;
    }
    else {
        make Term.new:
            lk($/,'ECMA262-Regex-atom').made,
            lk($/,'ECMA262-Regex-quantifier').?made # may be Nil
    }
}

method ECMA262-Regex-quantifier (Mu $/) {
    my $frugal = lk($/,'frugal').Str eq '?';
    my $prefix = lk($/,'ECMA262-Regex-quantifier-prefix');
    my $min;
    my $max;
    say "Frugal is $frugal";
    if    $prefix eq '?' { $min = 0; $max = 1   }
    elsif $prefix eq '+' { $min = 1; $max = Inf }
    elsif $prefix eq '*' { $min = 0; $max = Inf }
    else {
        with lk(lk($/,'ECMA262-Regex-quantifier-prefix'),'max') {
            $min = lk($prefix,'min').Str.Int;
            $max = lk($prefix,'max').Str.Int;
        } orwith lk($prefix,'range') {
            $min = lk($prefix,'min').Str.Int;
            $max = Inf;
        } else {
            $max = $min = lk($prefix,'min').Str.Int;
        }
    }
    make Quantifier.new(:$min, :$max, :$frugal);
}

proto method ECMA262-Regex-atom { * }
method ECMA262-Regex-atom:literal          (Mu $/) { make Literal.new($/.Str)                         }
method ECMA262-Regex-atom:any              (Mu $/) { make Wildcard                                    }
method ECMA262-Regex-atom:escape           (Mu $/) { make lk($/,'ECMA262-Regex-atom-escape').made     }
method ECMA262-Regex-atom:char-class       (Mu $/) { make lk($/,'ECMA262-Regex-character-class').made }
method ECMA262-Regex-atom:noncapture-group (Mu $/) { make Group.new: lk($/,'ECMA262-Regex-disjunction').made }
method ECMA262-Regex-atom:capture-group    (Mu $/) {
    make CaptureGroup.new: lk($/,'ECMA262-Regex-disjunction').made;
    # when there's a named one, register as this will be put in a header block.
    # This could (should?) maybe go in the grammar
    %*ECMA262-GROUP-NAMES{.made} = $*ECMA262-GROUP-COUNT with lk($/,'ECMA262-Regex-group-specifier');
}

method ECMA262-Regex-group-specifier (Mu $/) {
    make lk($/,'ECMA262-Regex-group-name').made
}
method ECMA262-Regex-group-name (Mu $/) {
    make lk($/,'ECMA262-Regex-regex-identifier-name').made
}
method ECMA262-Regex-regex-identifier-name (Mu $/) {
    make lk($/,'ECMA262-Regex-identifier-start-char').made
       ~ (.map(*.made).join with lk($/,'ECMA262-Regex-identifier-part-char'))
}
method ECMA262-Regex-identifier-start-char (Mu $/) {
    with lk($/,'ECMA262-Regex-unicode-escape-sequence')
        { make lk($/,'ECMA262-Regex-unicode-escape-sequence').made }
    else
        { make $/.Str }
}
method ECMA262-Regex-identifier-part-char (Mu $/) {
    with lk($/,'ECMA262-Regex-unicode-escape-sequence')
        { make lk($/,'ECMA262-Regex-unicode-escape-sequence').made}
    else
        { make $/.Str }
}


my %character-class-escape is Map =
    d => '<[0..9]>',
    D => '<-[0..9]>',
    w => '<[a..zA..Z_-]>',
    W => '<-[a..zA..Z_-]>',
    s => '<[\\x9\\xb\\xc\\xFEFF]+:Zs]>',
    S => '<-[\\x9\\xb\\xc\\xFEFF]-:Zs]>';

method ECMA262-Regex-atom-escape:decimal (Mu $/) {
    make BackreferencePositional.new: $/.Str
}
method ECMA262-Regex-atom-escape:named (Mu $/) {
    make BackreferenceNamed.new: lk($/,'ECMA262-Regex-group-name').made;
}
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
    'f', RakuAST::Regex::Literal.new("\xC"), # post space to avoid hex clash
    'n', RakuAST::Regex::Literal.new("\xA"),
    'r', RakuAST::Regex::Literal.new("\xD"),
    't', RakuAST::Regex::Literal.new("\x9"),
    'v', RakuAST::Regex::Literal.new("\xB")
;
method ECMA262-Regex-control-escape          (Mu $/) { make %control-escape.AT-KEY($/.Str)                         }
method ECMA262-Regex-control-letter          (Mu $/) { make RakuAST::Regex::Literal.new: ($/.lc.ord - 96).chr      }
method ECMA262-Regex-hex-escape-sequence     (Mu $/) { make RakuAST::Regex::Literal.new: $/.Str.parse-base(16).chr }
method ECMA262-Regex-unicode-escape-sequence (Mu $/) { make RakuAST::Regex::Literal.new: $/.Str.parse-base(16).chr }
method ECMA262-Regex-identity-escape         (Mu $/) { make RakuAST::Regex::Literal.new: $/.Str                    }
method ECMA262-Regex-unicode-property        (Mu $/) {

    say "did unicode property!";
    say "---> ", lk($/,'ECMA262-Regex-unicode-property-value-expression').made;
    make (lk($/,'type') eq 'p' ?? '<:' !! '<:!') ~  lk($/,'ECMA262-Regex-unicode-property-value-expression').made ~ '>'
}

method ECMA262-Regex-control-escape-in-char-class          (Mu $/) { make %control-escape.AT-KEY($/.Str)          }
method ECMA262-Regex-control-letter-in-char-class          (Mu $/) { make '\\x' ~ ($/.lc.ord - 96).base(16) ~ ' ' }
method ECMA262-Regex-hex-escape-sequence-in-char-class     (Mu $/) { make '\\x' ~ $/.Str ~ ' '                    }
method ECMA262-Regex-unicode-escape-sequence-in-char-class (Mu $/) { make '\\x' ~ $/.Str ~ ' '                    }
method ECMA262-Regex-identity-escape-in-char-class         (Mu $/) { make '\\'  ~ $/.Str                          }
method ECMA262-Regex-unicode-property-in-char-class        (Mu $/) {
    make (lk($/,'type') eq 'p' ?? ':' !! ':!') ~  lk($/,'ECMA262-Regex-unicode-property-value-expression').made ~ ' '
}

proto method ECMA262-Regex-assertion { * }
method ECMA262-Regex-assertion:str-start (Mu $/)        { make StartAnchor     }
method ECMA262-Regex-assertion:str-end (Mu $/)          { make EndAnchor       }
method ECMA262-Regex-assertion:start (Mu $/)            { make StartAnchor     }
method ECMA262-Regex-assertion:word-boundary (Mu $/)    { make WordBoundary.new: True  }
method ECMA262-Regex-assertion:neg-word-boundary (Mu $/){ make WordBoundary.new: False }
method ECMA262-Regex-assertion:pos-lookahead (Mu $/)    { make Lookahead.new: lk($/,'ECMA262-Regex-disjunction').made           }
method ECMA262-Regex-assertion:neg-lookahead (Mu $/)    { make Lookahead.new: lk($/,'ECMA262-Regex-disjunction').made, :negated }
method ECMA262-Regex-assertion:pos-lookback  (Mu $/)    { make Lookback.new:  lk($/,'ECMA262-Regex-disjunction').made           }
method ECMA262-Regex-assertion:neg-lookback  (Mu $/)    { make Lookback.new:  lk($/,'ECMA262-Regex-disjunction').made, :negated }

method ECMA262-Regex-character-class (Mu $/) {
    my $negated = so lk($/,'negated');
    my $inner = lk($/,'ECMA262-Regex-class-ranges').made;
    make CharacterClass.new(
        lk($/,'ECMA262-Regex-class-ranges').made,
        :$negated
    )
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
        ?? polarity($made) ~       $made       ~ (lk($/,'ECMA262-Regex-non-empty-class-ranges-no-dash').?made // '')
        !! polarity($made) ~ '[' ~ $made ~ ']' ~ (lk($/,'ECMA262-Regex-non-empty-class-ranges-no-dash').?made // '')
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
    make CharacterClassEscape.new($/.Str)
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
    make lk($/,'ECMA262-Regex-unicode-property-name').Str ~ '<' ~ lk($/,'ECMA262-Regex-unicode-property-value').Str ~ '>'
}