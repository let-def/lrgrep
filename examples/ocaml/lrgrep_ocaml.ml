module Menhir_grammar =
  MenhirSdk.Cmly_read.FromString(Ocaml_parser.Grammar_data)
module Grammar = Kernel.Info.Load_grammar(Menhir_grammar)

let find_token s = Result.get_ok (Kernel.Info.Terminal.find Grammar.grammar s)

let tAMPERAMPER                   = find_token "AMPERAMPER"
let tAMPERSAND                    = find_token "AMPERSAND"
let tAND                          = find_token "AND"
let tANDOP                        = find_token "ANDOP"
let tAS                           = find_token "AS"
let tASSERT                       = find_token "ASSERT"
(* let tAT                           = find_token "AT" *)
(* let tATAT                         = find_token "ATAT" *)
let tBACKQUOTE                    = find_token "BACKQUOTE"
let tBANG                         = find_token "BANG"
let tBAR                          = find_token "BAR"
let tBARBAR                       = find_token "BARBAR"
let tBARRBRACKET                  = find_token "BARRBRACKET"
let tBEGIN                        = find_token "BEGIN"
let tCHAR                         = find_token "CHAR"
let tCLASS                        = find_token "CLASS"
let tCOLON                        = find_token "COLON"
let tCOLONCOLON                   = find_token "COLONCOLON"
let tCOLONEQUAL                   = find_token "COLONEQUAL"
let tCOLONGREATER                 = find_token "COLONGREATER"
(* let tCOLONRBRACKET                = find_token "COLONRBRACKET" *)
let tCOMMA                        = find_token "COMMA"
let tCOMMENT                      = find_token "COMMENT"
let tCONSTRAINT                   = find_token "CONSTRAINT"
let tDO                           = find_token "DO"
let tDOCSTRING                    = find_token "DOCSTRING"
let tDONE                         = find_token "DONE"
let tDOT                          = find_token "DOT"
let tDOTDOT                       = find_token "DOTDOT"
(* let tDOTHASH                      = find_token "DOTHASH" *)
let tDOTOP                        = find_token "DOTOP"
let tDOWNTO                       = find_token "DOWNTO"
let tELSE                         = find_token "ELSE"
let tEND                          = find_token "END"
let tEOF                          = find_token "EOF"
let tEOL                          = find_token "EOL"
let tEQUAL                        = find_token "EQUAL"
let tEXCEPTION                    = find_token "EXCEPTION"
(* let tEXCLAVE                      = find_token "EXCLAVE" *)
let tEXTERNAL                     = find_token "EXTERNAL"
let tFALSE                        = find_token "FALSE"
let tFLOAT                        = find_token "FLOAT"
let tFOR                          = find_token "FOR"
let tFUN                          = find_token "FUN"
let tFUNCTION                     = find_token "FUNCTION"
let tFUNCTOR                      = find_token "FUNCTOR"
(* let tGLOBAL                       = find_token "GLOBAL" *)
let tGREATER                      = find_token "GREATER"
let tGREATERRBRACE                = find_token "GREATERRBRACE"
let tGREATERRBRACKET              = find_token "GREATERRBRACKET"
let tHASH                         = find_token "HASH"
(* let tHASH_FLOAT                   = find_token "HASH_FLOAT" *)
(* let tHASH_INT                     = find_token "HASH_INT" *)
(* let tHASHLBRACE                   = find_token "HASHLBRACE" *)
(* let tHASHLPAREN                   = find_token "HASHLPAREN" *)
let tHASHOP                       = find_token "HASHOP"
(* let tHASH_SUFFIX                  = find_token "HASH_SUFFIX" *)
let tIF                           = find_token "IF"
let tIN                           = find_token "IN"
let tINCLUDE                      = find_token "INCLUDE"
let tINFIXOP0                     = find_token "INFIXOP0"
let tINFIXOP1                     = find_token "INFIXOP1"
let tINFIXOP2                     = find_token "INFIXOP2"
let tINFIXOP3                     = find_token "INFIXOP3"
let tINFIXOP4                     = find_token "INFIXOP4"
let tINHERIT                      = find_token "INHERIT"
let tINITIALIZER                  = find_token "INITIALIZER"
let tINT                          = find_token "INT"
(* let tKIND_ABBREV                  = find_token "KIND_ABBREV" *)
(* let tKIND_OF                      = find_token "KIND_OF" *)
let tLABEL                        = find_token "LABEL"
let tLAZY                         = find_token "LAZY"
let tLBRACE                       = find_token "LBRACE"
let tLBRACELESS                   = find_token "LBRACELESS"
let tLBRACKET                     = find_token "LBRACKET"
let tLBRACKETAT                   = find_token "LBRACKETAT"
let tLBRACKETATAT                 = find_token "LBRACKETATAT"
let tLBRACKETATATAT               = find_token "LBRACKETATATAT"
let tLBRACKETBAR                  = find_token "LBRACKETBAR"
(* let tLBRACKETCOLON                = find_token "LBRACKETCOLON" *)
let tLBRACKETGREATER              = find_token "LBRACKETGREATER"
let tLBRACKETLESS                 = find_token "LBRACKETLESS"
let tLBRACKETPERCENT              = find_token "LBRACKETPERCENT"
let tLBRACKETPERCENTPERCENT       = find_token "LBRACKETPERCENTPERCENT"
let tLESS                         = find_token "LESS"
let tLESSMINUS                    = find_token "LESSMINUS"
let tLET                          = find_token "LET"
let tLETOP                        = find_token "LETOP"
let tLIDENT                       = find_token "LIDENT"
(* let tLOCAL                        = find_token "LOCAL" *)
let tLPAREN                       = find_token "LPAREN"
let tMATCH                        = find_token "MATCH"
let tMETHOD                       = find_token "METHOD"
let tMINUS                        = find_token "MINUS"
let tMINUSDOT                     = find_token "MINUSDOT"
let tMINUSGREATER                 = find_token "MINUSGREATER"
(* let tMOD                          = find_token "MOD" *)
let tMODULE                       = find_token "MODULE"
let tMUTABLE                      = find_token "MUTABLE"
let tNEW                          = find_token "NEW"
let tNONREC                       = find_token "NONREC"
let tOBJECT                       = find_token "OBJECT"
let tOF                           = find_token "OF"
(* let tONCE                         = find_token "ONCE" *)
let tOPEN                         = find_token "OPEN"
let tOPTLABEL                     = find_token "OPTLABEL"
let tOR                           = find_token "OR"
(* let tOVERWRITE                    = find_token "OVERWRITE" *)
let tPERCENT                      = find_token "PERCENT"
let tPLUS                         = find_token "PLUS"
let tPLUSDOT                      = find_token "PLUSDOT"
let tPLUSEQ                       = find_token "PLUSEQ"
let tPREFIXOP                     = find_token "PREFIXOP"
let tPRIVATE                      = find_token "PRIVATE"
let tQUESTION                     = find_token "QUESTION"
let tQUOTE                        = find_token "QUOTE"
let tQUOTED_STRING_EXPR           = find_token "QUOTED_STRING_EXPR"
let tQUOTED_STRING_ITEM           = find_token "QUOTED_STRING_ITEM"
let tRBRACE                       = find_token "RBRACE"
let tRBRACKET                     = find_token "RBRACKET"
let tREC                          = find_token "REC"
let tRPAREN                       = find_token "RPAREN"
let tSEMI                         = find_token "SEMI"
let tSEMISEMI                     = find_token "SEMISEMI"
let tSIG                          = find_token "SIG"
(* let tSTACK                        = find_token "STACK" *)
let tSTAR                         = find_token "STAR"
let tSTRING                       = find_token "STRING"
let tSTRUCT                       = find_token "STRUCT"
let tTHEN                         = find_token "THEN"
let tTILDE                        = find_token "TILDE"
let tTO                           = find_token "TO"
let tTRUE                         = find_token "TRUE"
let tTRY                          = find_token "TRY"
let tTYPE                         = find_token "TYPE"
let tUIDENT                       = find_token "UIDENT"
let tUNDERSCORE                   = find_token "UNDERSCORE"
(* let tUNIQUE                       = find_token "UNIQUE" *)
let tVAL                          = find_token "VAL"
let tVIRTUAL                      = find_token "VIRTUAL"
let tWHEN                         = find_token "WHEN"
let tWHILE                        = find_token "WHILE"
let tWITH                         = find_token "WITH"
let tMETAOCAML_ESCAPE             = find_token "METAOCAML_ESCAPE"
let tMETAOCAML_BRACKET_OPEN       = find_token "METAOCAML_BRACKET_OPEN"
let tMETAOCAML_BRACKET_CLOSE      = find_token "METAOCAML_BRACKET_CLOSE"
let tEFFECT                       = find_token "EFFECT"

let terminal_of_token = function
  | Ocaml_parser.Parser.AMPERAMPER -> tAMPERAMPER
  | AMPERSAND                  -> tAMPERSAND
  | AND                        -> tAND
  | ANDOP _                    -> tANDOP
  | AS                         -> tAS
  | ASSERT                     -> tASSERT
  (* | AT                         -> tAT *)
  (* | ATAT                       -> tATAT *)
  | BACKQUOTE                  -> tBACKQUOTE
  | BANG                       -> tBANG
  | BAR                        -> tBAR
  | BARBAR                     -> tBARBAR
  | BARRBRACKET                -> tBARRBRACKET
  | BEGIN                      -> tBEGIN
  | CHAR _                     -> tCHAR
  | CLASS                      -> tCLASS
  | COLON                      -> tCOLON
  | COLONCOLON                 -> tCOLONCOLON
  | COLONEQUAL                 -> tCOLONEQUAL
  | COLONGREATER               -> tCOLONGREATER
  (* | COLONRBRACKET              -> tCOLONRBRACKET *)
  | COMMA                      -> tCOMMA
  | COMMENT _                  -> tCOMMENT
  | CONSTRAINT                 -> tCONSTRAINT
  | DO                         -> tDO
  | DOCSTRING _                -> tDOCSTRING
  | DONE                       -> tDONE
  | DOT                        -> tDOT
  | DOTDOT                     -> tDOTDOT
  (* | DOTHASH                    -> tDOTHASH *)
  | DOTOP _                    -> tDOTOP
  | DOWNTO                     -> tDOWNTO
  | ELSE                       -> tELSE
  | END                        -> tEND
  | EOF                        -> tEOF
  | EOL                        -> tEOL
  | EQUAL                      -> tEQUAL
  | EXCEPTION                  -> tEXCEPTION
  (* | EXCLAVE                    -> tEXCLAVE *)
  | EXTERNAL                   -> tEXTERNAL
  | FALSE                      -> tFALSE
  | FLOAT _                    -> tFLOAT
  | FOR                        -> tFOR
  | FUN                        -> tFUN
  | FUNCTION                   -> tFUNCTION
  | FUNCTOR                    -> tFUNCTOR
  (* | GLOBAL                     -> tGLOBAL *)
  | GREATER                    -> tGREATER
  | GREATERRBRACE              -> tGREATERRBRACE
  | GREATERRBRACKET            -> tGREATERRBRACKET
  | HASH                       -> tHASH
  (* | HASH_FLOAT _               -> tHASH_FLOAT *)
  (* | HASH_INT _                 -> tHASH_INT *)
  (* | HASHLBRACE                 -> tHASHLBRACE *)
  (* | HASHLPAREN                 -> tHASHLPAREN *)
  | HASHOP _                   -> tHASHOP
  (* | HASH_SUFFIX                -> tHASH_SUFFIX *)
  | IF                         -> tIF
  | IN                         -> tIN
  | INCLUDE                    -> tINCLUDE
  | INFIXOP0 _                 -> tINFIXOP0
  | INFIXOP1 _                 -> tINFIXOP1
  | INFIXOP2 _                 -> tINFIXOP2
  | INFIXOP3 _                 -> tINFIXOP3
  | INFIXOP4 _                 -> tINFIXOP4
  | INHERIT                    -> tINHERIT
  | INITIALIZER                -> tINITIALIZER
  | INT _                      -> tINT
  (* | KIND_ABBREV                -> tKIND_ABBREV *)
  (* | KIND_OF                    -> tKIND_OF *)
  | LABEL _                    -> tLABEL
  | LAZY                       -> tLAZY
  | LBRACE                     -> tLBRACE
  | LBRACELESS                 -> tLBRACELESS
  | LBRACKET                   -> tLBRACKET
  | LBRACKETAT                 -> tLBRACKETAT
  | LBRACKETATAT               -> tLBRACKETATAT
  | LBRACKETATATAT             -> tLBRACKETATATAT
  | LBRACKETBAR                -> tLBRACKETBAR
  (* | LBRACKETCOLON              -> tLBRACKETCOLON *)
  | LBRACKETGREATER            -> tLBRACKETGREATER
  | LBRACKETLESS               -> tLBRACKETLESS
  | LBRACKETPERCENT            -> tLBRACKETPERCENT
  | LBRACKETPERCENTPERCENT     -> tLBRACKETPERCENTPERCENT
  | LESS                       -> tLESS
  | LESSMINUS                  -> tLESSMINUS
  | LET                        -> tLET
  | LETOP _                    -> tLETOP
  | LIDENT _                   -> tLIDENT
  (* | LOCAL                      -> tLOCAL *)
  | LPAREN                     -> tLPAREN
  | MATCH                      -> tMATCH
  | METHOD                     -> tMETHOD
  | MINUS                      -> tMINUS
  | MINUSDOT                   -> tMINUSDOT
  | MINUSGREATER               -> tMINUSGREATER
  (* | MOD                        -> tMOD *)
  | MODULE                     -> tMODULE
  | MUTABLE                    -> tMUTABLE
  | NEW                        -> tNEW
  | NONREC                     -> tNONREC
  | OBJECT                     -> tOBJECT
  | OF                         -> tOF
  (* | ONCE                       -> tONCE *)
  | OPEN                       -> tOPEN
  | OPTLABEL _                 -> tOPTLABEL
  | OR                         -> tOR
  (* | OVERWRITE                  -> tOVERWRITE *)
  | PERCENT                    -> tPERCENT
  | PLUS                       -> tPLUS
  | PLUSDOT                    -> tPLUSDOT
  | PLUSEQ                     -> tPLUSEQ
  | PREFIXOP _                 -> tPREFIXOP
  | PRIVATE                    -> tPRIVATE
  | QUESTION                   -> tQUESTION
  | QUOTE                      -> tQUOTE
  | QUOTED_STRING_EXPR _       -> tQUOTED_STRING_EXPR
  | QUOTED_STRING_ITEM _       -> tQUOTED_STRING_ITEM
  | RBRACE                     -> tRBRACE
  | RBRACKET                   -> tRBRACKET
  | REC                        -> tREC
  | RPAREN                     -> tRPAREN
  | SEMI                       -> tSEMI
  | SEMISEMI                   -> tSEMISEMI
  | SIG                        -> tSIG
  (* | STACK                      -> tSTACK *)
  | STAR                       -> tSTAR
  | STRING _                   -> tSTRING
  | STRUCT                     -> tSTRUCT
  | THEN                       -> tTHEN
  | TILDE                      -> tTILDE
  | TO                         -> tTO
  | TRUE                       -> tTRUE
  | TRY                        -> tTRY
  | TYPE                       -> tTYPE
  | UIDENT _                   -> tUIDENT
  | UNDERSCORE                 -> tUNDERSCORE
  (* | UNIQUE                     -> tUNIQUE *)
  | VAL                        -> tVAL
  | VIRTUAL                    -> tVIRTUAL
  | WHEN                       -> tWHEN
  | WHILE                      -> tWHILE
  | WITH                       -> tWITH
  | METAOCAML_ESCAPE           -> tMETAOCAML_ESCAPE
  | METAOCAML_BRACKET_OPEN     -> tMETAOCAML_BRACKET_OPEN
  | METAOCAML_BRACKET_CLOSE    -> tMETAOCAML_BRACKET_CLOSE
  | EFFECT                     -> tEFFECT

let () =
  let first_lex = ref true in
  Lrgrep_top.run_custom_lrgrep
    ~language_name:"ocaml"
    ~parser_module_name:"Parser"
    ~grammar:Grammar.grammar
    ~sentence_lexer:(fun ic ->
        if ic == stdin then (
          if !first_lex then (
            output_string stdout "Write an OCaml sentence (prefix).\n";
            output_string stdout "Start with interface: to parse it as an interface.\n";
            output_string stdout "Submit with Ctrl-D.\n";
            first_lex := false;
          );
          output_string stdout "$ ";
        flush stdout;
        );
        Lexer.init ();
        let lexbuf = Lexing.from_channel ~with_positions:true ic in
        Lexing.set_filename lexbuf "<lrgrep-interpreter>";
        let rec drain acc =
          match Ocaml_parser.Lexer.token lexbuf with
          | Ocaml_parser.Parser.EOF -> List.rev acc
          | tok -> drain ((tok, lexbuf.lex_start_p, lexbuf.lex_curr_p) :: acc)
        in
        let ep, tokens =
          match drain [] with
          | (Ocaml_parser.Parser.LIDENT ("implementation" | "interface" as ep), startp, endp) ::
            (Ocaml_parser.Parser.COLON, _, _) :: rest ->
            (Some (ep, startp, endp), rest)
          | [] -> raise End_of_file
          | tokens -> (None, tokens)
        in
        (ep, List.map (fun (t,s,p) -> (terminal_of_token t,s,p)) tokens)
      )
    ()
