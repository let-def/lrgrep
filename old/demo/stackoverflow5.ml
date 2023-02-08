(* https://stackoverflow.com/questions/70691144/match-inside-match-ocaml-raises-syntax-error *)

type 'a grid = 'a Array.t Array.t

type problem = { initial_grid : int option grid }

type available = { loc : int * int; possible : int list }

type state = { problem : problem; current_grid : int option grid; available = available list }

let branch_state (state : state) : (state * state) option =
  if prazni_kvadratki state.current_grid = [] then
    None
  else
    let lst = prazni_kvadratki state.current_grid in
    let loc = List.hd lst in
    let st1_grid = copy_grid state.current_grid in
    let st2_grid = copy_grid state.current_grid in
    match razpolozljive state.current_grid loc with
    | x :: xs -> (vstavi_vrednost st1_grid loc (Some x);
                  let st1 = {problem = state.problem; current_grid = st1_grid} in
                  match xs with
                  | [y] -> (vstavi_vrednost st2_grid loc (Some y);
                           let st2 = {
                             problem = state.problem;
                             current_grid = st2_grid
                           })   (* this is where it shows me a syntax error*)
                  | y :: ys -> let st2 = {
                      problem = state.problem;
                      current_grid = copy_grid state.current_grid;
                      available = {loc = loc; possible = xs}
                    })
Some (st1, st2)
