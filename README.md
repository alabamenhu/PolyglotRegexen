# Polyglot::Regexen

A module to enable use of non-Raku flavors of regex directly in Raku code.

### Why?

Because copy and paste.  Also, there are lots of little gotchas when going between different flavors.  What counts for `\s`, `\d`, `.`, or even `$` can vary substantially or in quite subtle ways. 
If you have a battle-tested regex, don't worry about rewriting it. Just use it as is!

The goal of this module is to provide a variety of ways for you to use your favor flavors in Raku scripts — even in grammars!  
When using regexen from this module, you can be assured that capture ordering is preserved, the meaning of whitespace won't get changed, etc.

## Support

The support for different flavors is shown below in a table, followed by a discussion on any (hopefully temporary) deviations from the standard.
The meaning of the columns is the following:
  * **In grammars**: these can be used inside of grammars.  Just declare the regex with the prefix.  Since `}` will necessarily be the terminating character, you may need to escape it in some way.
  * **Bare quoted**: just as you can use `m/foo/` for regular regexes, these allow you to prefix with, e.g., `ecma-m/foo/`
  * **Lexical scope**: these are defined by `my`/`our` scoping, e.g. `my ecma-regex { … }`  

| Flavor                | In grammars | Bare quoted (substitution) | Lexical scoped | prefix |
|-----------------------|:-----------:|:--------------------------:|:--------------:|:------:|
| ECMA-262 (Javascript) |      ✓      |           ✓ (𐄂)           |       𐄂       |  ecma  |
| Python                |     𐄂      |          𐄂 (𐄂)           |       𐄂       |   py   |
| Ruby                  |     𐄂      |          𐄂 (𐄂)           |       𐄂       |  ruby  |
| PHP                   |     𐄂      |          𐄂 (𐄂)           |       𐄂       |  php   |

(At the moment, only Javascript is supported.  The others are aspirational.)

### Support Caveats

* ECMA-262 (Javascript)  
  * Not all options are currently available (right now, just `s`, `m`, and `i`).  
  * In block form (defined with `ecma-regex { … }`, leading and trailing white space is ignored).
  * To add options in block form, use a colon: `ecma-regex:im { … }`
  * Named references (via `\\k`) not yet supported
  * Unicode option is effectively always on.  More testing will be required to tease out if/when that makes a difference
  
  
# Version History
  * **v0.0.1
    * Initial public release with most of ECMA262 (JavaScript) regex supported