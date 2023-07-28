use v6.d;
unit module Polyglot::Regex::ECMA262::Classes;
use experimental :rakuast;

sub rx-block-blockoid-stmtlist (+@list) {
    RakuAST::Regex::Block.new(RakuAST::Block.new(
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new: @list)
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
            ?? RakuAST::Regex::Backtrack::Frugal.new
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
    has @!items;
    method new(+@items) { self.bless: :@items }
    method GENERATE {
        RakuAST::Regex::Group.new(@!items)
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
        RakuAST::Regex::NamedCapture.new(
            name  => $!pos.Str,
            regex => RakuAST::Regex::Group.new(@!items)
        ),
        rx-block-blockoid-stmtlist(
            RakuAST::Statement::Expression.new(
                expression => RakuAST::ApplyPostfix.new(
                    operand => RakuAST::Var::Lexical.new('$¢'),
                    postfix => RakuAST::Call::Method.new(
                        name => RakuAST::Name.from-identifier('register-position'),
                        args => RakuAST::ArgList.new([
                            RakuAST::ApplyPostfix.new(
                                operand => RakuAST::Var::Lexical.new('$/'),
                                postfix => RakuAST::Call::Method.new(
                                    name => RakuAST::Name.from-identifier-parts(<Match AT-POS>),
                                    args => RakuAST::ArgList.new([RakuAST::IntLiteral.new: $*ECMA262-GROUP-COUNT])
                                )
                            ),
                            RakuAST::IntLiteral.new($*ECMA262-GROUP-COUNT)
                        ])
                    )
                )
            )
        )
    }
}

class BackreferencePositional is export {
    has $.position;
    method new($position) { self.bless: :$position}
    method GENERATE {
        # This is tricky here.  It doesn't seem like RakuAST yet supports the
        # `$(...)` syntax for doing a literal return of a code block.
        # So instead, we'll use <{ (...).raku }> which should interpolate
        # correctly as a literal string.  What we want is
        #     [$($/.AT-POS: 2)]
        # So we generate
        #     <{ $/.AT-POS(2).Str.raku }>
        RakuAST::Regex::Assertion::InterpolatedBlock.new(
            block => RakuAST::Block.new(
                body => RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyPostfixi.new(
                                operand => RakuAST::ApplyPostfix.new(
                                    operand => RakuAST::ApplyPostfix.new(
                                        operand => RakuAST::Var::Lexical.new('$¢'),
                                        postfix => RakuAST::Call::Method.new(
                                            name => RakuAST::Name.from-identifier('AT-POS'),
                                            args => RakuAST::ArgList.new([RakuAST::IntLiteral.new: $!position])
                                        ),
                                    ),
                                    postfix => RakuAST::Call::Method.new(
                                        name => RakuAST::Name.from-identifier('Str')
                                    )
                                ),
                                postfix => RakuAST::Name.from-identifier('raku')
                            )
                        )
                    )
                )
            )
        )
    }
}

class BackreferenceNamed is export {
    has $.name;
    method new($name) { self.bless: :$name}
    method GENERATE {
        # This is tricky here.  It doesn't seem like RakuAST yet supports the
        # `$(...)` syntax for doing a literal return of a code block.
        # So instead, we'll use <{ (...).raku }> which should interpolate
        # correctly as a literal string.  What we want is
        #     [$($/.AT-KEY: 'foo')]
        # So we generate
        #     <{ $/.AT-KEY('foo').Str.raku }>
        RakuAST::Regex::Assertion::InterpolatedBlock.new(
            block => RakuAST::Block.new(
                body => RakuAST::Blockoid.new(
                    RakuAST::StatementList.new(
                        RakuAST::Statement::Expression.new(
                            expression => RakuAST::ApplyPostfixi.new(
                                operand => RakuAST::ApplyPostfix.new(
                                    operand => RakuAST::ApplyPostfix.new(
                                        operand => RakuAST::Var::Lexical.new('$¢'),
                                        postfix => RakuAST::Call::Method.new(
                                            name => RakuAST::Name.from-identifier('AT-KEY'),
                                            args => RakuAST::ArgList.new([RakuAST::StrLiteral.new: $!name])
                                        ),
                                    ),
                                    postfix => RakuAST::Call::Method.new(
                                        name => RakuAST::Name.from-identifier('Str')
                                    )
                                ),
                                postfix => RakuAST::Name.from-identifier('raku')
                            )
                        )
                    )
                )
            )
        )
    }
}

constant ECMA262-WordChar = RakuAST::Regex::Assertion::CharClass.new(
    RakuAST::Regex::CharClassElement::Enumeration.new(
        elements => [
            RakuAST::Regex::CharClassEnumerationElement::Character.new("_"),
            RakuAST::Regex::CharClassEnumerationElement::Range.new(:from("a".ord),:to("z".ord)),
            RakuAST::Regex::CharClassEnumerationElement::Range.new(:from("A".ord),:to("Z".ord)),
            RakuAST::Regex::CharClassEnumerationElement::Range.new(:from("0".ord),:to("9".ord)),
        ]
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
                    ?? @!items.head
                    !! RakuAST::Regex::Sequence.new(@!items)
                ),
               :$!negated
            )
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
                    ?? @!items.head
                    !! RakuAST::Regex::Sequence.new(@!items)
                ),
                :$!negated
            )
        )
    }
}

class CharacterClassEscape {...}
class CharacterClass is export {
    has $.negated;
    has @.items;
    method new (+@items, :$negated = False) { self.bless: :@items, :$negated }
    method GENERATE {
        # The dynamic variable here is needed to let the character classes such as
        # \s or \S know whether they should be reversed.  THis is because we can't use
        # them directly, and while I can use [^a\S] (not a and not NOT a space) in JavaScript
        # that must be <-[a]+[spacechars]>.
        my $*NEGATED = $!negated;
        my @flip; # these need inversion
        my @base; # these don't

        for @!items -> $item {
            if ($item ~~ CharacterClassEscape) && $item.negated {
                @flip.push: $item
            } else {
                @base.push: $item
            }
        }

        my $base := @base
            ?? RakuAST::Regex::CharClassElement::Enumeration.new(
                elements => @base.map(*.GENERATE),
                :$!negated
            )
            !! Empty;
        my $flip := @flip
            ?? RakuAST::Regex::CharClassElement::Enumeration.new(
                elements => @flip.map(*.GENERATE),
                negated => !$!negated
            )
            !! Empty;
        RakuAST::Regex::Assertion::CharClass.new([$base, $flip])
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
    method new($start, $end) { self.bless: :$start, :$end }
    method GENERATE {
        RakuAST::Regex::CharClassEnumerationElement::Character.new(
            Range.new(:from($!start.ord),:to($!end.ord))
        )
    }
}

class CharacterClassEscape is export {
    has $.type;
    has $.negated;
    method new($type) {
        self.bless: :type($type.lc), :negated($type eq $type.lc)
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