use v6.d;
#use MONKEY-GUTS;
use nqp; use QAST:from<NQP>;

unit role ECMA262-Regex-Actions-Mixin;
    use Polyglot::Regex::ECMA262::Actions;
    also does ECMA262-Regex-Actions;

my sub lk(Mu \match, \key) { match.hash.AT-KEY: key }

method routine_declarator:sym<ecma-regex> (Mu $/) {
    use MONKEY-SEE-NO-EVAL;

    if $*PKGDECL                    # are we in a package?
    && $*PKGDECL eq  'grammar'      # is the package a grammar?
    && $*PACKAGE =:= $*DECLARAND    # is the grammar our current scope?
        {
            my $name  := lk($/,'name').Str;
            my $regex := lk($/,'ECMA262-Regex-TOP').made;
            $*MULTINESS
                ?? $*PACKAGE.^add_multi_method($name, $regex)
                !! $*PACKAGE.^add_method($name, $regex)
        }
    else # we are basically an our scoped declarator
        {
            my $name  := lk($/,'name').Str;
            my $regex := lk($/,'ECMA262-Regex-TOP').made;
            my $block := $*W.cur_lexpad();
            $*W.add_object_if_no_sc($regex);
            say $*ERR, 'inside a scoped declarator: ', $*SCOPE;

            $*W.install_lexical_container(
                $block,
                $name,
                Nil,
                $regex,
                :scope( $*SCOPE ?? $*SCOPE !! ($*PACKAGE ?? 'has' !! 'our' )),
                :package($*PACKAGE),
            );
            #make QAST::Op.new(
            #            :op('bind'),
            #            QAST::Var.new(:name('&' ~ $name), :scope('lexical'), :decl('var')),
             #           QAST::WVal.new(:value($regex)),
                        #'&x'
                        #$*W.symbol_lookup([$name], $/, :package_only(1), :lvalue(1))
            #);
            #
            $block.push:
                QAST::Op.new(
                :op('bind'),
                    QAST::Var.new(:name($name), :scope('lexical')),
                    QAST::WVal.new(:value($regex))
                );
            $block.push:
                QAST::Op.new(
                :op('say'),
                    QAST::SVal.new(:value('$regex'))
                );
            $block.push:
                QAST::Op.new(
                :op('say'),
                    QAST::Var.new(:name($name), :scope('lexical'))
                );

        }
}

method term:sym<regex_ecma_quote> (Mu $/) {
    my sub lk(Mu \match, \key) { match.hash.AT-KEY: key }
     my $ecma-regex := lk($/,'ECMA262-Regex-TOP').made;
    #`<<<
    my $block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
    self.handle_and_check_adverbs($/, %SHARED_ALLOWED_ADVERBS, 'rx', $block);
    my %sig_info := hash(parameters => []);
    my $coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
        $<quibble>.ast, 'anon', '', %sig_info, $block) if $<quibble>.ast;
    my $past := block_closure($coderef, :regex);
    $past.annotate('sink_ast', QAST::Op.new(:op<callmethod>, :name<Bool>, $past));
    make $past;>>>

    #make QAST::WVal.new(:value(Foo.new))
    $*W.add_object_if_no_sc($ecma-regex);
    make QAST::WVal.new(:value($ecma-regex));
}
