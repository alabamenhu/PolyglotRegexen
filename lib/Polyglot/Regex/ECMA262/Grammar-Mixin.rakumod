use v6.d;

unit role ECMA262-Regex-Mixin;
    use Polyglot::Regex::ECMA262::Grammar:auth<zef:guifa>;
    also does ECMA262-Regex;

#rule statement_control:sym<m:bin> {}
my sub lk(Mu \match, \key) { match.hash.AT-KEY: key }

token routine_declarator:sym<ecma-regex> {
    :my $*LINE_NO := {
        use QAST:from<NQP>; # allows access to HLL::Compiler, not strictly necessary though
        HLL::Compiler.lineof(self.orig(), self.from(), :cache(1))
    };
    :my $*ECMA262-SPACE-FLANKED := True;
    :my $*ECMA262-ALLOW-POSTOPTIONS := False;

    # These need to be "activated" here, even though <ECMA262-Regex-TOP> will as well,
    # because we need to have the values ready at the point of that token's action.
    :my $*ECMA262-DOT-MATCHES-NEWLINE = False;
    :my $*ECMA262-CASE-INSENSITIVE    = False;
    :my $*ECMA262-MULTILINE-MODE      = False;
    :my $*FOO = 42;

    # What we want to parse is this format:     ecma-regex:s foo (…) { <code> }
    <sym> <.end_keyword>                      # ecma-regex
    <ECMA262-Regex-options('decl')>           #           :s
    <.ws>                                     #
    $<name> = <.identifier> <.ws>             #              foo
    [ # Signatures                            #
       '('                                    #                  (…)
       <.panic('no ecma signatures for now')> #
    ]?                                        #
    <.ws>                                     #
    '{'                                       #                      {
      :my $*ECMA262-ENDER := '}';             #
      <ECMA262-Regex-TOP>                     #                        <code>
    \s* '}'                                   #                               }
    <?ENDSTMT>

    # The <?ENDSTMT> allows the final } to end a line without a semicolon
}

token term:sym<regex_ecma_quote> {
        :my $*ECMA262-ALLOW-POSTOPTIONS = True;
        :my $*ECMA262-HAD-POSTOPTIONS   = False;
        :my $*ECMA262-ENDER := '/';

        :my $*ECMA262-CASE-INSENSITIVE  = False;

        'ecma-m'
        #<ECMA262-Regex-options('quot')>
        '/'
        <ECMA262-Regex-TOP>
        [
        || <?{$*ECMA262-HAD-POSTOPTIONS}>                               # slash already consumed
        || '/'                                                          # consume the slash
        || <.panic: "Unable to parse regex; couldn't find final '/'">   # wtf was this
        ]
} # $*W.install_lexical_container. It takes a :scope

#`<<<<<

token term:sym<regex_ecma_quote> {
        'ecma'
        {say $*ERR, " - current offset: ", $/.pos;}
        :my $*ECMA262-ENDER := '/';
        {say $*ERR, ' - matched ecma text'}
        <ECMA262-Regex-options>
        '/'
        <ECMA262-Regex-TOP>
        {say " - completed regex top"}
        [ '/' || <.panic: "Unable to parse regex; couldn't find final '/'"> ]
} # $*W.install_lexical_container. It takes a :scope
>>>>>

#|to be handled later
token ECMA262-Regex-options ($*ECMA262-Regex-options-type) {
    #\h*
    [':'
        [
        | <ECMA262-Regex-option>+
        | <.panic('Unknown or missing modifier for ECMA 262 regex.  Did you mean :i or :s?')>
        ]
    ]?
}
token ECMA262-Regex-option {
    || <[ism]>
       { # TODO: make this prettier
            if $/.Str eq 'i' { $*ECMA262-CASE-INSENSITIVE    = True }
            if $/.Str eq 'm' { $*ECMA262-MULTILINE-MODE      = True }
            if $/.Str eq 's' { $*ECMA262-DOT-MATCHES-NEWLINE = True }
       }
    || <[dguy]> <.panic("Modifier '{$/.Str}' not yet supported or not possible here")>
    || <alpha> <.panic("Unknown modifier '{$/.AT-KEY('alpha')}'on ECMA regex")>
    || \S <.panic("Regex modifier must be a letter.  Currently supported modifiers are <i s m> with support coming for <d g u y>")>
}