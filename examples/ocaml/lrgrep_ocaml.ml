module Menhir_grammar = MenhirSdk.Cmly_read.FromString(Grammar_data)
module Grammar = Kernel.Info.Load_grammar(Menhir_grammar)

let () = Lrgrep_top.run_custom_lrgrep
    ~language_name:"ocaml"
    ~parser_module_name:"Parser"
    ~grammar:Grammar.grammar
    ~string_of_terminal:
    ()
