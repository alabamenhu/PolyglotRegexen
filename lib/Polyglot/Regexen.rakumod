use v6.d;

sub EXPORT(|) {
    use Polyglot::Regex::ECMA262::Grammar-Mixin:auth<zef:guifa>;
    use Polyglot::Regex::ECMA262::Actions-Mixin:auth<zef:guifa>;

    # This mixes in our grammar in the main language.
    $*LANG.define_slang:
            'MAIN',
            $*LANG.slang_grammar('MAIN').^mixin(ECMA262-Regex-Mixin),
            $*LANG.slang_actions('MAIN').^mixin(ECMA262-Regex-Actions-Mixin);
    Map.new
}

