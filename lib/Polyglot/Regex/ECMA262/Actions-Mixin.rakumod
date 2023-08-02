use v6.d;
#use MONKEY-GUTS;
use nqp; use QAST:from<NQP>;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

unit role ECMA262-Regex-Actions-Mixin;
    use Polyglot::Regex::ECMA262::Actions:auth<zef:guifa>;
    also does ECMA262-Regex-Actions;

my sub lk(Mu \match, \key) { match.hash.AT-KEY: key }

method routine_declarator:sym<regex:ecma> (Mu $/) {
    # Detect options
    my %adverbs;
    with lk($/,'i') { %adverbs<adverbs> = (RakuAST::ColonPair::True.new("i") ) }

    if $*PKGDECL                    # are we in a package?
    && $*PKGDECL eq  'grammar'      # is the package a grammar?
    && $*PACKAGE =:= $*DECLARAND    # is the grammar our current scope?
        {
            # This SHOULD use RakuAST::RegexDeclaration.new(...)
            # However, it requires there to be an attach target
            # surrounding it *in the AST* which for now we don't have.
            my $name  := lk($/,'name').Str;
            my $regex := EVAL RakuAST::QuotedRegex.new(
               #name => RakuAST::Name.from-identifier($name),
                body => lk($/,'ECMA262-Regex-TOP').made,
                |%adverbs
            );

            $*MULTINESS
                ?? $*PACKAGE.^add_multi_method($name, $regex)
                !! $*PACKAGE.^add_method($name, $regex)
        }
    else # we are basically an our scoped declarator
        {

            my $name  := lk($/,'name').Str;
            my $regex := EVAL RakuAST::QuotedRegex.new(
               #name => RakuAST::Name.from-identifier($name),
                body => lk($/,'ECMA262-Regex-TOP').made,
                |%adverbs
            );
            my $block := $*W.cur_lexpad();
            $*W.add_object_if_no_sc($regex);

            $*W.install_lexical_container(
                $block,
                '&' ~ $name,
                Nil,
                $regex,
                :scope($*SCOPE ?? $*SCOPE !! ($*PACKAGE ?? 'has' !! 'our' )),
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
                    QAST::Var.new(:name('&' ~ $name), :scope('lexical')), #:scope('lexical')),
                    QAST::WVal.new(:value($regex))
                );
        }
}

method term:sym<regex_ecma_quote> (Mu $/) {
    my sub lk(Mu \match, \key) { match.hash.AT-KEY: key }

    # Capture adverbs that can't be simply compiled in
    my %adverbs;
    %adverbs<adverbs> = (RakuAST::ColonPair::True.new("i") ) if $*ECMA262-CASE-INSENSITIVE;
    my $ecma-regex := EVAL RakuAST::QuotedRegex.new(body => lk($/,'ECMA262-Regex-TOP').made, |%adverbs);
    #`<<<
    my $block := QAST::Block.new(QAST::Stmts.new, QAST::Stmts.new, :node($/));
    self.handle_and_check_adverbs($/, %SHARED_ALLOWED_ADVERBS, 'rx', $block);
    my %sig_info := hash(parameters => []);
    my $coderef := regex_coderef($/, $*W.stub_code_object('Regex'),
        $<quibble>.ast, 'anon', '', %sig_info, $block) if $<quibble>.ast;
    my $past := block_closure($coderef, :regex);
    $past.annotate('sink_ast', QAST::Op.new(:op<callmethod>, :name<Bool>, $past));
    make $past;>>>

    $*W.add_object_if_no_sc($ecma-regex);
    make QAST::WVal.new(:value($ecma-regex));
}

method term:sym<regex_ecma_match> (Mu $/) {
    my sub lk(Mu \match, \key) { match.hash.AT-KEY: key }
    my %adverbs;
    %adverbs<adverbs> = (RakuAST::ColonPair::True.new("i") ) if $*ECMA262-CASE-INSENSITIVE;
    say lk($/,'ECMA262-Regex-TOP').made;
    say "*" x 144;
    my $ecma-regex := EVAL RakuAST::QuotedRegex.new(
            body => lk($/,'ECMA262-Regex-TOP').made,
            |%adverbs
    );


    $*W.add_object_if_no_sc($ecma-regex);
    my $foo = QAST::Op.new(
        :op<callmethod>,
        :name<ACCEPTS>,
        QAST::WVal.new(:value($ecma-regex)),
        QAST::Var.new(:scope<lexical>, :name<$_>),
    );
    $*W.add_object_if_no_sc($foo);

    #make QAST::WVal.new(:value($ecma-regex));
    #make QAST::WVal.new(:value($foo));
    make $foo;
}
