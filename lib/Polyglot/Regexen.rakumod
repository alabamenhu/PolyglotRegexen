use v6.d;

sub EXPORT(|) {
    use Polyglot::Regex::ECMA262::Grammar-Mixin;
    use Polyglot::Regex::ECMA262::Actions-Mixin;

    # This mixes in our grammar in the main language.
    # Ideally, we should a slang called 'BASIC' and provide a grammar and action class
    # (rather than mixing in roles), and use the 'official' way of a <nibble> which
    # switches the current language, which would avoid any potential overlap/redefinition
    # of symbols.  That's not possible at the moment, though :-(
    $*LANG.define_slang:
            'MAIN',
            $*LANG.slang_grammar('MAIN').^mixin(ECMA262-Regex-Mixin),
            $*LANG.slang_actions('MAIN').^mixin(ECMA262-Regex-Actions-Mixin);
    Map.new
}

