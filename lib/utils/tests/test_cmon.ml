open Utils
open Cmon

let print_doc x = PPrint.ToChannel.pretty 0.9 80 stdout x
let print_t x = print_doc (print_as_is x)

(* Test basic constructions *)

let%expect_test _ = print_t @@
  unit;
  [%expect {| () |}]

let%expect_test _ = print_t @@
  bool false;
  [%expect {| false |}]

let%expect_test _ = print_t @@
  bool true;
  [%expect {| true |}]

let%expect_test _ = print_t @@
  int 0;
  [%expect {| 0 |}]

let%expect_test _ = print_t @@
  int (-16384);
  [%expect {| -16384 |}]

let%expect_test _ = print_t @@
  float 123.45;
  [%expect {| 123.45 |}]

let%expect_test _ = print_t @@
  string "hello";
  [%expect {| "hello" |}]

let%expect_test _ = print_t @@
  string "\"quote\"";
  [%expect {| "\"quote\"" |}]

let%expect_test _ = print_t @@
  constant "None";
  [%expect {| None |}]

let%expect_test _ = print_t @@
  constructor "Some" (string "body");
  [%expect {| Some "body" |}]

let%expect_test _ = print_t @@
  tuple [bool false; int 0; string ""];
  [%expect {| (false, 0, "") |}]

let%expect_test _ = print_t @@
  record ["x", float 0.0; "y", float 1.0; "z", float 2.0];
  [%expect {| { x = 0.; y = 1.; z = 2. } |}]

let%expect_test _ = print_t @@
  ctuple "Lam" [string "x"; ctuple "App" [string "x"; string "y"]];
  [%expect {| Lam ("x", App ("x", "y")) |}]

let%expect_test _ = print_t @@
  crecord "Lam" ["var", string "x";
                 "body", crecord "App" ["functional", string "x"; "argument", string "y"]];
  [%expect {| Lam { var = "x"; body = App { functional = "x"; argument = "y" } } |}]

let parsetree_chunk =
  let dummy_pos = record [
      "pos_fname", string "";
      "pos_lnum", int 0;
      "pos_bol", int 0;
      "pos_cnum", int (-1);
    ]
  in
  let loc_none = record [
      "loc_start", dummy_pos;
      "loc_end", dummy_pos;
      "loc_ghost", bool true;
    ]
  in
  let loc x = record ["txt", x; "loc", loc_none] in
  let typ desc = record [
      "ptyp_desc", desc;
      "ptyp_loc", loc_none;
      "ptyp_loc_stack", nil;
      "ptyp_attributes", nil;
    ]
  and pattern desc = record [
      "ppat_desc", desc;
      "ppat_loc", loc_none;
      "ppat_loc_stack", nil;
      "ppat_attributes", nil;
    ]
  and expr desc = record [
      "pexp_desc", desc;
      "pexp_loc", loc_none;
      "pexp_loc_stack", nil;
      "pexp_attributes", nil;
    ]
  in
  let shared_type = typ @@ ctuple "Ptyp_constr" [
      loc (constructor "Lident" (string "int"));
      nil
    ]
  in
  constructor "Ptop_def" @@ list [
    record [
      "pstr_desc",
      ctuple "Pstr_value" [
        constant "Nonrec";
        list [
          record [
            "pvb_pat", pattern @@ ctuple "Ppat_constraint" [
              pattern (constructor "Ppat_var" (loc (string "x")));
              shared_type;
            ];
            "pvb_expr", expr @@ ctuple "Pexp_constraint" [
              expr (constructor "Pexp_constant" (
                  ctuple "Pconst_integer" [
                    string "1";
                    constant "None";
                  ]
                ));
              shared_type
            ];
            "pvb_attributes", nil;
            "pvb_loc", loc_none;
          ]
        ]
      ];
      "pstr_loc", loc_none;
    ];
  ]

let%expect_test _ =
  print_endline "(* Without sharing *)";
  print_t parsetree_chunk;
  [%expect {|
    (* Without sharing *)
    Ptop_def [
      {
        pstr_desc =
          Pstr_value (
            Nonrec,
            [
              {
                pvb_pat =
                  {
                    ppat_desc =
                      Ppat_constraint (
                        {
                          ppat_desc =
                            Ppat_var {
                              txt = "x";
                              loc =
                                {
                                  loc_start =
                                    {
                                      pos_fname = "";
                                      pos_lnum = 0;
                                      pos_bol = 0;
                                      pos_cnum = -1
                                    };
                                  loc_end =
                                    {
                                      pos_fname = "";
                                      pos_lnum = 0;
                                      pos_bol = 0;
                                      pos_cnum = -1
                                    };
                                  loc_ghost = true
                                }
                            };
                          ppat_loc =
                            {
                              loc_start =
                                {
                                  pos_fname = "";
                                  pos_lnum = 0;
                                  pos_bol = 0;
                                  pos_cnum = -1
                                };
                              loc_end =
                                {
                                  pos_fname = "";
                                  pos_lnum = 0;
                                  pos_bol = 0;
                                  pos_cnum = -1
                                };
                              loc_ghost = true
                            };
                          ppat_loc_stack = [];
                          ppat_attributes = []
                        },
                        {
                          ptyp_desc =
                            Ptyp_constr (
                              {
                                txt = Lident "int";
                                loc =
                                  {
                                    loc_start =
                                      {
                                        pos_fname = "";
                                        pos_lnum = 0;
                                        pos_bol = 0;
                                        pos_cnum = -1
                                      };
                                    loc_end =
                                      {
                                        pos_fname = "";
                                        pos_lnum = 0;
                                        pos_bol = 0;
                                        pos_cnum = -1
                                      };
                                    loc_ghost = true
                                  }
                              },
                              []
                            );
                          ptyp_loc =
                            {
                              loc_start =
                                {
                                  pos_fname = "";
                                  pos_lnum = 0;
                                  pos_bol = 0;
                                  pos_cnum = -1
                                };
                              loc_end =
                                {
                                  pos_fname = "";
                                  pos_lnum = 0;
                                  pos_bol = 0;
                                  pos_cnum = -1
                                };
                              loc_ghost = true
                            };
                          ptyp_loc_stack = [];
                          ptyp_attributes = []
                        }
                      );
                    ppat_loc =
                      {
                        loc_start =
                          {
                            pos_fname = "";
                            pos_lnum = 0;
                            pos_bol = 0;
                            pos_cnum = -1
                          };
                        loc_end =
                          {
                            pos_fname = "";
                            pos_lnum = 0;
                            pos_bol = 0;
                            pos_cnum = -1
                          };
                        loc_ghost = true
                      };
                    ppat_loc_stack = [];
                    ppat_attributes = []
                  };
                pvb_expr =
                  {
                    pexp_desc =
                      Pexp_constraint (
                        {
                          pexp_desc = Pexp_constant (Pconst_integer ("1", None));
                          pexp_loc =
                            {
                              loc_start =
                                {
                                  pos_fname = "";
                                  pos_lnum = 0;
                                  pos_bol = 0;
                                  pos_cnum = -1
                                };
                              loc_end =
                                {
                                  pos_fname = "";
                                  pos_lnum = 0;
                                  pos_bol = 0;
                                  pos_cnum = -1
                                };
                              loc_ghost = true
                            };
                          pexp_loc_stack = [];
                          pexp_attributes = []
                        },
                        {
                          ptyp_desc =
                            Ptyp_constr (
                              {
                                txt = Lident "int";
                                loc =
                                  {
                                    loc_start =
                                      {
                                        pos_fname = "";
                                        pos_lnum = 0;
                                        pos_bol = 0;
                                        pos_cnum = -1
                                      };
                                    loc_end =
                                      {
                                        pos_fname = "";
                                        pos_lnum = 0;
                                        pos_bol = 0;
                                        pos_cnum = -1
                                      };
                                    loc_ghost = true
                                  }
                              },
                              []
                            );
                          ptyp_loc =
                            {
                              loc_start =
                                {
                                  pos_fname = "";
                                  pos_lnum = 0;
                                  pos_bol = 0;
                                  pos_cnum = -1
                                };
                              loc_end =
                                {
                                  pos_fname = "";
                                  pos_lnum = 0;
                                  pos_bol = 0;
                                  pos_cnum = -1
                                };
                              loc_ghost = true
                            };
                          ptyp_loc_stack = [];
                          ptyp_attributes = []
                        }
                      );
                    pexp_loc =
                      {
                        loc_start =
                          {
                            pos_fname = "";
                            pos_lnum = 0;
                            pos_bol = 0;
                            pos_cnum = -1
                          };
                        loc_end =
                          {
                            pos_fname = "";
                            pos_lnum = 0;
                            pos_bol = 0;
                            pos_cnum = -1
                          };
                        loc_ghost = true
                      };
                    pexp_loc_stack = [];
                    pexp_attributes = []
                  };
                pvb_attributes = [];
                pvb_loc =
                  {
                    loc_start =
                      { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1 };
                    loc_end =
                      { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1 };
                    loc_ghost = true
                  }
              }
            ]
          );
        pstr_loc =
          {
            loc_start = { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1 };
            loc_end = { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1 };
            loc_ghost = true
          }
      }
    ] |}]

let%expect_test _ =
  print_endline "(* With sharing *)";
  print_t (explicit_sharing parsetree_chunk);
  [%expect {|
    (* With sharing *)
    Ptop_def [
      let v20 =
        let v19 = { pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = -1 }
        in
        { loc_start = v19; loc_end = v19; loc_ghost = true }
      in
      {
        pstr_desc =
          Pstr_value (
            Nonrec,
            [
              let v25 = {
                  ptyp_desc = Ptyp_constr ({ txt = Lident "int"; loc = v20 }, []);
                  ptyp_loc = v20;
                  ptyp_loc_stack = [];
                  ptyp_attributes = []
                }
              in
              {
                pvb_pat =
                  {
                    ppat_desc =
                      Ppat_constraint (
                        {
                          ppat_desc = Ppat_var { txt = "x"; loc = v20 };
                          ppat_loc = v20;
                          ppat_loc_stack = [];
                          ppat_attributes = []
                        },
                        v25
                      );
                    ppat_loc = v20;
                    ppat_loc_stack = [];
                    ppat_attributes = []
                  };
                pvb_expr =
                  {
                    pexp_desc =
                      Pexp_constraint (
                        {
                          pexp_desc = Pexp_constant (Pconst_integer ("1", None));
                          pexp_loc = v20;
                          pexp_loc_stack = [];
                          pexp_attributes = []
                        },
                        v25
                      );
                    pexp_loc = v20;
                    pexp_loc_stack = [];
                    pexp_attributes = []
                  };
                pvb_attributes = [];
                pvb_loc = v20
              }
            ]
          );
        pstr_loc = v20
      }
    ] |}]
