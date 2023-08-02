use v6.d;
unit module Polyglot::Regex::ECMA262::Classes;
use experimental :rakuast;

sub rx-block-blockoid-stmtlist (+@list) {
    RakuAST::Regex::Block.new(RakuAST::Block.new(
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new: |@list)
    ))
}

class Disjunction is export {
    has @.alternatives;
    method new(+@alternatives) {
        return @alternatives.head if @alternatives == 1;
        self.bless: :@alternatives;
    }
    method GENERATE {
        RakuAST::Regex::SequentialAlternation.new: |@!alternatives.map(*.GENERATE)
    }
}

class Alternative is export {
    has @.terms;
    method new(+@terms) {
        return @terms.head if @terms == 1;
        self.bless: :@terms;
    }
    method GENERATE {
        RakuAST::Regex::Sequence.new(|@!terms.map(*.GENERATE))
    }
}

class Quantifier is export {...}
class Term is export {
    has $.atom is required;
    has $.quantifier;
    method new($atom, $quantifier?) {
        self.bless: :$atom, :$quantifier
    }
    method GENERATE {
        return $!atom.GENERATE unless $!quantifier;
        # captures require special handling here if quantified
        # if $!atom ~~ Capture ...
        RakuAST::Regex::QuantifiedAtom.new(
            atom => $!atom.GENERATE,
            quantifier => $!quantifier.GENERATE
        )
    }
}

class Quantifier is export {
    has $.min;
    has $.max;
    has $.frugal = False;
    method GENERATE {
        my $backtrack := $!frugal
            ?? RakuAST::Regex::Backtrack::Frugal
            !! Empty;
        $!min == 0
            ?? $!max == Inf
                ?? RakuAST::Regex::Quantifier::ZeroOrMore.new(:$backtrack)
                !! $!max == 1
                    ?? RakuAST::Regex::Quantifier::ZeroOrOne.new(:$backtrack)
                    !! RakuAST::Regex::Quantifier::Range.new(
                           :$!min,
                           :$backtrack,
                           |(:$!max if $!max < Inf),

                       )
            !! $!min == 1 && $!max == Inf
                ?? RakuAST::Regex::Quantifier::OneOrMore.new(:$backtrack)
                !! RakuAST::Regex::Quantifier::Range.new(
                       :$!min,
                       :$backtrack,
                       |(:$!max if $!max < Inf),
                   )
    }
}

class Literal is export {
    has $.value;
    method new($value) {
        self.bless: :$value
    }
    method GENERATE {
        RakuAST::Regex::Literal.new($!value)
    }
}

class Wildcard is export {
    method GENERATE {
        # $*dot-newline  # via the /s flag
        #   ?? /./       # Raku matches newlines by default
        #   !! /<-[\n]>/ # ECMA matches all but newlines by default
        $*ECMA262-DOT-MATCHES-NEWLINE
            ?? RakuAST::Regex::CharClass::Any.new
            !! RakuAST::Regex::Assertion::CharClass.new(
                   RakuAST::Regex::CharClassElement::Enumeration.new(
                       :negated,
                       elements => [RakuAST::Regex::CharClass::Newline.new,]
                   )
               )
    }
}

class Group is export {
    has @.items;
    method new(+@items) { self.bless: :@items }
    method GENERATE {
        RakuAST::Regex::Group.new(|@!items.map(*.GENERATE))
    }
}

class CaptureGroup is export {
    has $.pos;
    has $.name;
    has @.items;

    method new(+@items, :$pos, :$name) {
        self.bless: :@items, :$pos, :$name
    }
    method GENERATE {
        # %*ECMA262-GROUP-NAMES{.made} = $*ECMA262-GROUP-COUNT with lk($/,'ECMA262-Regex-group-specifier');
        # This is effectively equivalent to
        #     $2=[...]{$¢.register-position: $2, 2}
        # However, $2 doesn't work in code because we'll have applied
        # the role that overrides AT-POS. So the full breakdown is
        #     $2=[...]{$¢.register-position: $/.Match::AT-POS(2), 2}
        # When quantifying, this original should be used, followed by
        # a non-capturing version of its contents quantified one less
        # time. If quantifying as ?, simply insert into a basic group
        # for most simplicity.
        my @stmts;
        @stmts.push: RakuAST::Regex::NamedCapture.new(
            name  => $!pos.Str,
            regex => RakuAST::Regex::Group.new(|@!items.map(*.GENERATE))
        );
        @stmts.push: rx-block-blockoid-stmtlist(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyPostfix.new(
                    operand => RakuAST::Var::Lexical.new('$¢'),
                    postfix => RakuAST::Call::Method.new(
                        name => RakuAST::Name.from-identifier('register-position'),
                        args => RakuAST::ArgList.new(
                            RakuAST::ApplyPostfix.new(
                                operand => RakuAST::Var::Lexical.new('$/'),
                                postfix => RakuAST::Call::Method.new(
                                    # name => RakuAST::Name.from-identifier-parts('Match', 'AT-POS'), <-- should be
                                    name => RakuAST::Name.from-identifier('AT-POS'), # <-- should be
                                    args => RakuAST::ArgList.new(
                                        RakuAST::IntLiteral.new($!pos),
                                        RakuAST::ColonPair::True.new("ECMA262-INTERNAL") # <-- shouldn't be necessary
                                    )
                                )
                            ),
                            RakuAST::IntLiteral.new($!pos)
                        )
                    )
                )
            )
        );

        RakuAST::Regex::Group.new( RakuAST::Regex::Sequence.new: |@stmts )
    }
}

# TODO: JavaScript does not support forward references, but does not treat them as an error.
# In JavaScript, forward references always find a zero-length match, just as backreferences
# to non-participating groups do in JavaScript. Because this is not particularly useful,
# XRegExp makes them an error. In std::regex, Boost, Python, Tcl, and VBScript forward
# references are an error.
class BackreferencePositional is export {
    has $.position;
    method new($position) { self.bless: :$position}
    method GENERATE {
        RakuAST::Regex::Interpolation.new(
            sequential => False,
            var => RakuAST::Contextualizer::Item.new( # RakuAST::StatementSequence.new(
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::ApplyPostfix.new(
                        operand => RakuAST::Var::Lexical.new('$¢'),
                        postfix => RakuAST::Call::Method.new(
                            name => RakuAST::Name.from-identifier('AT-POS'),
                            args => RakuAST::ArgList.new(
                                RakuAST::IntLiteral.new: $!position.Int
                            )
                        )
                    )
                )
            ) # )
        )
    }
}

class BackreferenceNamed is export {
    has $.name;
    method new($name) { self.bless: :$name}
    method GENERATE {
        RakuAST::Regex::Interpolation.new(
            sequential => False,
            var => RakuAST::Contextualizer::Item.new( # RakuAST::StatementSequence.new(
                RakuAST::Statement::Expression.new(
                    expression => RakuAST::ApplyPostfix.new(
                        operand => RakuAST::Var::Lexical.new('$¢'),
                        postfix => RakuAST::Call::Method.new(
                            name => RakuAST::Name.from-identifier('AT-KEY'),
                            args => RakuAST::ArgList.new(
                                RakuAST::StrLiteral.new: $!name
                            )
                        )
                    )
                )
            ) # )
        )
    }
}

my \elements = RakuAST::Regex::CharClassEnumerationElement::Character.new("_"),
            RakuAST::Regex::CharClassEnumerationElement::Range.new(:from("a".ord),:to("z".ord)),
            RakuAST::Regex::CharClassEnumerationElement::Range.new(:from("A".ord),:to("Z".ord)),
            RakuAST::Regex::CharClassEnumerationElement::Range.new(:from("0".ord),:to("9".ord));
constant ECMA262-WordChar = RakuAST::Regex::Assertion::CharClass.new(
    RakuAST::Regex::CharClassElement::Enumeration.new(
        elements => elements
    )
);
constant ECMA262-WordCharNeg = RakuAST::Regex::Assertion::CharClass.new(
    RakuAST::Regex::CharClassElement::Enumeration.new(
        elements => [
            RakuAST::Regex::CharClassEnumerationElement::Character.new("_"),
            RakuAST::Regex::CharClassEnumerationElement::Range.new(:from("a".ord),:to("z".ord)),
            RakuAST::Regex::CharClassEnumerationElement::Range.new(:from("A".ord),:to("Z".ord)),
            RakuAST::Regex::CharClassEnumerationElement::Range.new(:from("0".ord),:to("9".ord)),
        ],
        :negated
    )
);

class WordBoundary is export {
    # The word boundary in ECMA is defined as a word boundary.
    # The problem is that it defines a word character as simply [a-zA-Z0-9_],
    # thus /\b./ would match 'b' and NOT 'a' given 'ábc'.  The best Raku
    # equivalent is
    # [
    # | <after <+[a..zA..Z0..9_]>> <before <-[a..zA..Z0..9_]>>
    # | <after <-[a..zA..Z0..9_]>> <before <+[a..zA..Z0..9_]>>
    # ]
    has $.affirmative;
    method new($affirmative = True) {
        self.bless: :$affirmative
    }
    method GENERATE {
        RakuAST::Regex::Group.new(
            RakuAST::Regex::SequentialAlternation.new([
                RakuAST::Regex::Sequence.new([
                    RakuAST::Regex::Assertion::Lookahead.new(
                        assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                            name => RakuAST::Name.from-identifier('after'),
                            regex-arg => ECMA262-WordCharNeg,
                        )
                    ),
                    RakuAST::Regex::Assertion::Lookahead.new(
                        assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                            name => RakuAST::Name.from-identifier('before'),
                            regex-arg => ($!affirmative ?? ECMA262-WordChar !! ECMA262-WordCharNeg),
                        )
                    )
                ]),
                RakuAST::Regex::Sequence.new([
                    RakuAST::Regex::Assertion::Lookahead.new(
                        assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                            name => RakuAST::Name.from-identifier('after'),
                            regex-arg => ECMA262-WordChar,
                        )
                    ),
                    RakuAST::Regex::Assertion::Lookahead.new(
                        assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                            name => RakuAST::Name.from-identifier('before'),
                            regex-arg => ($!affirmative ?? ECMA262-WordCharNeg !! ECMA262-WordChar),
                        )
                    )
                ]),
            ])
        )
    }
}

class StartAnchor is export {
    method GENERATE {
        $*ECMA262-MULTILINE-MODE
            ?? RakuAST::Regex::Anchor::BeginningOfLine
            !! RakuAST::Regex::Anchor::BeginningOfString
    }
}
class EndAnchor is export {
    method GENERATE {
        $*ECMA262-MULTILINE-MODE
            ?? RakuAST::Regex::Anchor::EndOfLine
            !! RakuAST::Regex::Anchor::EndOfString
    }
}

class Lookahead is export {
    has @.items;
    has $.negated;
    method new (+@items, :$negated = False) { self.bless: :@items, :$negated }
    method GENERATE {
        RakuAST::Regex::Assertion::Lookahead.new(
            assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                name => RakuAST::Name.from-identifier('before'),
                regex-arg => (@!items == 1
                    ?? @!items.head.GENERATE
                    !! RakuAST::Regex::Sequence.new(@!items.map: *.GENERATE)
                ),
            ),
            :$!negated
        )
    }
}

class Lookback is export {
    has @.items;
    has $.negated;
    method new (+@items, :$negated = False) { self.bless: :@items, :$negated }
    method GENERATE {
        RakuAST::Regex::Assertion::Lookahead.new(
            assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                name => RakuAST::Name.from-identifier('after'),
                regex-arg => (@!items == 1
                    ?? @!items.head.GENERATE
                    !! RakuAST::Regex::Sequence.new(@!items.map: *.GENERATE)
                ),
            ),
            :$!negated
        )
    }
}

class CharacterClassEscape {...}
class CharacterClassUnicodeProperty {...}

class CharacterClass is export {
    has $.negated;
    has @.items;
    method new (+@items, :$negated = False) { self.bless: :@items, :$negated }
    method GENERATE {
        # ECMA262 allows an empty character class.  Raku doesn't, but it's basically an ignorable
        return Empty unless @!items;

        # The dynamic variable here is needed to let the character classes such as
        # \s or \S know whether they should be reversed.  THis is because we can't use
        # them directly, and while I can use [^a\S] (not a and not NOT a space) in JavaScript
        # that must be <-[a]+[spacechars]>.
        my $*NEGATED = $!negated;
        my @flip; # these need inversion
        my @base; # these don't
        my @flip-u; # these need inversion
        my @base-u; # these don't

        for @!items -> $item {
            if ($item ~~ CharacterClassEscape) && $item.negated {
                @flip.push: $item
            } else {
                $item ~~ CharacterClassUnicodeProperty
                    ?? @base-u.push($item)
                    !! @base.push($item)
            }
        }

        # A given generator may produce multiple values, so we need to flatten before passing
        my $base := @base
            ?? RakuAST::Regex::CharClassElement::Enumeration.new(
                :$!negated,
                :elements(@base.map(*.GENERATE).flat.Array)
            )
            !! Empty;
        my $flip := @flip
            ?? RakuAST::Regex::CharClassElement::Enumeration.new(
                :elements(@flip.map(*.GENERATE).flat.Array),
                negated => !$!negated
            )
            !! Empty;
        my $base-u := @base-u
            ?? @base-u.map(*.GENERATE)
            !! Empty;

        RakuAST::Regex::Assertion::CharClass.new(|$base, |$flip, |$base-u)
    }
}

class CharacterClassSingle is export {
    has $.char;
    method new($char) { self.bless: :$char }
    method GENERATE {
        RakuAST::Regex::CharClassEnumerationElement::Character.new($!char)
    }
}

class CharacterClassRange is export {
    has $.start;
    has $.end;
    proto method new(|) {*}
    multi method new(CharacterClassSingle $start, CharacterClassSingle $end) { samewith $start.char, $end.char }
    multi method new(Str $start, Str $end) { samewith $start.ord, $end.ord }
    multi method new(Int $start, Int $end) { self.bless: :$start, :$end }
    multi method new(Any :$start, Any :$end) { samewith $start, $end }
    method GENERATE {
        RakuAST::Regex::CharClassEnumerationElement::Range.new(
            :from($!start),:to($!end)
        )
    }
}

class CharacterClassEscape is export {
    has $.type;
    has $.negated;
    method new($type) {
        self.bless: :type($type.lc), :negated($type eq $type.uc)
    }
    method GENERATE {
        # dDsSwW
        given $!type {
            when 'd' {
                RakuAST::Regex::CharClassEnumerationElement::Range.new(
                    :from('0'.ord), :to('9'.ord)
                )
            }
            when 's' {
                # +[\x9\xb\xc\xFEFF]+:Zs
                RakuAST::Regex::CharClassEnumerationElement::Character.new("\x9"),
                RakuAST::Regex::CharClassEnumerationElement::Character.new("\xB"),
                RakuAST::Regex::CharClassEnumerationElement::Character.new("\xC"),
                RakuAST::Regex::CharClassEnumerationElement::Character.new("\xFEFF"),
                # should be this, but negating it is more complicate
                #     RakuAST::Regex::CharClassElement::Property.new(property => 'Zs')
                # so we manually do it (6 characters plus a range)
                RakuAST::Regex::CharClassEnumerationElement::Character.new("\x20"),
                RakuAST::Regex::CharClassEnumerationElement::Character.new("\xA0"),
                RakuAST::Regex::CharClassEnumerationElement::Character.new("\x1680"),
                RakuAST::Regex::CharClassEnumerationElement::Range.new(
                    :from(0x2000), :to(0x200A)
                ),
                RakuAST::Regex::CharClassEnumerationElement::Character.new("\x202F"),
                RakuAST::Regex::CharClassEnumerationElement::Character.new("\x205F"),
                RakuAST::Regex::CharClassEnumerationElement::Character.new("\x3000"),
            }
            when 'w' {
                # a..zA..Z_-
                RakuAST::Regex::CharClassEnumerationElement::Range.new(
                    :from('a'.ord), :to('z'.ord)
                ),
                RakuAST::Regex::CharClassEnumerationElement::Range.new(
                    :from('A'.ord), :to('Z'.ord)
                ),
                RakuAST::Regex::CharClassEnumerationElement::Character.new("_"),
            }
        }
    }
}

constant %unicode-names =
    ASCII => 'ASCII',
    ASCII_Hex_Digit  => 'ASCII_Hex_Digit', AHex => 'ASCII_Hex_Digit',
    Alphabetic  => 	'Alphabetic',Alpha =>  'Alphabetic',
    Any  => 	'Any',
    Assigned 	 => 'Assigned',
    Bidi_Control  => 	'Bidi_Control', Bidi_C  => 'Bidi_Control',
    Bidi_Mirrored  => 	'Bidi_Mirrored', Bidi_M  => 'Bidi_Mirrored',
    Case_Ignorable  => 	'Case_Ignorable',   CI  => 'Case_Ignorable',
    Cased  => 	'Cased',
    Changes_When_Casefolded  => 	'Changes_When_Casefolded', CWCF  => 'Changes_When_Casefolded',
    Changes_When_Casemapped 	 => 'Changes_When_Casemapped', CWCM  => 'Changes_When_Casemapped',
    Changes_When_Lowercased  => 	'Changes_When_Lowercased', CWL  => 'Changes_When_Lowercased',
    Changes_When_NFKC_Casefolded  => 	'Changes_When_NFKC_Casefolded', CWKCF =>  'Changes_When_NFKC_Casefolded',
    Changes_When_Titlecased  => 	'Changes_When_Titlecased', CWT => 	'Changes_When_Titlecased',
    Changes_When_Uppercased => 	'Changes_When_Uppercased', CWU => 'Changes_When_Uppercased',
    Dash => 	'Dash',
    Default_Ignorable_Code_Point => 	'Default_Ignorable_Code_Point', DI => 'Default_Ignorable_Code_Point',
    Deprecated => 	'Deprecated', Dep => 'Deprecated',
    Diacritic => 	'Diacritic', Dia => 'Diacritic',
    Emoji => 	'Emoji',
    Emoji_Component => 	'Emoji_Component', EComp => 'Emoji_Component',
    Emoji_Modifier => 	'Emoji_Modifier',EMod => 'Emoji_Modifier',
    Emoji_Modifier_Base => 	'Emoji_Modifier_Base', EBase => 'Emoji_Modifier_Base',
    Emoji_Presentation => 	'Emoji_Presentation', EPres => 'Emoji_Presentation',
    Extended_Pictographic => 	'Extended_Pictographic', ExtPict => 'Extended_Pictographic',
    Extender => 	'Extender', Ext => 'Extender',
    Grapheme_Base => 	'Grapheme_Base', Gr_Base => 'Grapheme_Base',
    Grapheme_Extend => 	'Grapheme_Extend', Gr_Ext => 'Grapheme_Extend',
    Hex_Digit => 	'Hex_Digit', Hex => 'Hex_Digit',
    IDS_Binary_Operator => 	'IDS_Binary_Operator', IDSB => 'IDS_Binary_Operator',
    IDS_Trinary_Operator => 	'IDS_Trinary_Operator', IDST => 'IDS_Trinary_Operator',
    ID_Continue => 	'ID_Continue', IDC => 'ID_Continue',
    ID_Start => 	'ID_Start', IDS => 'ID_Start',
    Ideographic 	=> 'Ideographic', Ideo => 'Ideographic',
    Join_Control => 	'Join_Control', Join_C=>  'Join_Control',
    Logical_Order_Exception => 	'Logical_Order_Exception', LOE => 'Logical_Order_Exception',
    Lowercase => 	'Lowercase', Lower => 'Lowercase',
    Math  =>	'Math',
    Noncharacter_Code_Point  =>	'Noncharacter_Code_Point', NChar  =>'Noncharacter_Code_Point',
    Pattern_Syntax  =>	'Pattern_Syntax', Pat_Syn  =>'Pattern_Syntax',
    Pattern_White_Space  =>	'Pattern_White_Space', Pat_WS  =>'Pattern_White_Space',
    Quotation_Mark  =>	'Quotation_Mark', QMark  =>'Quotation_Mark',
    Radical  =>	'Radical',
    Regional_Indicator  =>	'Regional_Indicator', RI  =>'Regional_Indicator',
    Sentence_Terminal  =>	'Sentence_Terminal', STerm  =>'Sentence_Terminal',
    Soft_Dotted 	 =>'Soft_Dotted', SD  =>'Soft_Dotted',
    Terminal_Punctuation  =>	'Terminal_Punctuation', Term  =>'Terminal_Punctuation',
    Unified_Ideograph  =>	'Unified_Ideograph', UIdeo  =>'Unified_Ideograph',
    Uppercase  =>	'Uppercase', Upper => 'Uppercase',
    Variation_Selector  =>	'Variation_Selector', VS  =>'Variation_Selector',
    White_Space  =>	'White_Space', space  =>'White_Space',
    XID_Continue  =>	'XID_Continue', XIDC => 'XID_Continue',
    XID_Start  =>	'XID_Start', XIDS  =>'XID_Start';

class CharacterClassUnicodeProperty is export {
    has $.property;
    has $.inverted;
    has $.predicate;
    method new(:$property, :$inverted, :$predicate) {
        self.bless: :$property, :$inverted, :$predicate
    }
    method GENERATE {
        my $negated = $*NEGATED;
        if $!predicate {
            RakuAST::Regex::CharClassElement::Property.new(
                :$!property,
                :predicate(
                    RakuAST::Circumfix::Parentheses.new(
                        RakuAST::SemiList.new(
                            RakuAST::Statement::Expression.new(
                                expression => RakuAST::QuotedString.new(
                                    segments   => (RakuAST::StrLiteral.new($!predicate),)
                                )
                            )
                        )
                    )
                ),
                :$!inverted,
                :$negated
            )

        } else {
            RakuAST::Regex::CharClassElement::Property.new(
                :$!property,
                :$!inverted,
                :$negated
            )
        }
    }
}


# Unlike the character class, this one can only be inverted
class UnicodeProperty is export {
    has $.property;
    has $.inverted;
    has $.predicate;
    method new(:$property, :$predicate, :$inverted) {
        self.bless: :$property, :$predicate, :$inverted
    }
    method GENERATE {
        if $!predicate {
            RakuAST::Regex::Assertion::CharClass.new(
                RakuAST::Regex::CharClassElement::Property.new(
                    :$!property,
                    :predicate(
                        RakuAST::Circumfix::Parentheses.new(
                            RakuAST::SemiList.new(
                                RakuAST::Statement::Expression.new(
                                    expression => RakuAST::QuotedString.new(
                                        segments   => (RakuAST::StrLiteral.new($!predicate),)
                                    )
                                )
                            )
                        )
                    ),
                    :$!inverted,
                ),
            )
        } else {
            RakuAST::Regex::Assertion::CharClass.new(
                RakuAST::Regex::CharClassElement::Property.new(
                    :$!property,
                    :$!inverted,
                )
            )
        }
    }
}

class AtomicCharacterClassEscape is export {
    has $.char-class-escape;
    method new($type) {
        self.bless: char-class-escape => (CharacterClassEscape.new: $type)
    }
    method GENERATE {
        RakuAST::Regex::Assertion::CharClass.new(
            RakuAST::Regex::CharClassElement::Enumeration.new(
                elements => $!char-class-escape.GENERATE.Array,
                negated => $!char-class-escape.negated
            )
        )
    }

}