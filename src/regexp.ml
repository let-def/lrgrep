open Utils
open BitSet
open Misc

open Grammar

module RE = struct
  type uid = int

  let uid = let k = ref 0 in fun () -> incr k; !k

  type var = int * int

  type t = { uid: uid; desc: desc; position: Syntax.position }

  and desc =
    | Set of Lr1.set * var option
    | Alt of t list
    | Seq of t list
    | Star of t
    | Reduce

  let make position desc = {uid = uid (); desc; position}

  let compare t1 t2 =
    Int.compare t1.uid t2.uid

  let cmon ?(var=fun (x,y) -> Cmon.tuple [Cmon.int x; Cmon.int y]) t =
    let rec aux t =
      match t.desc with
      | Set (lr1s, v) ->
        Cmon.construct "Set" [
          Cmon.constant
            ("{" ^ string_of_int (IndexSet.cardinal lr1s) ^ " states}");
          match v with
          | None -> Cmon.constant "None"
          | Some x -> Cmon.constructor "Some" (var x)
        ]
      | Alt ts -> Cmon.constructor "Alt" (Cmon.list_map aux ts)
      | Seq ts -> Cmon.constructor "Seq" (Cmon.list_map aux ts)
      | Star t -> Cmon.constructor "Star" (aux t)
      | Reduce -> Cmon.constant "Reduce"
    in
    aux t
end

module KRE = struct
  type t =
    | Done of {clause: int}
    | More of RE.t * t

  let rec cmon = function
    | Done {clause} -> Cmon.constructor "Done" (Cmon.int clause)
    | More (re, t) ->
      Cmon.cons (RE.cmon re) (cmon t)

  let more re t = More (re, t)

  let rec compare k1 k2 =
    match k1, k2 with
    | Done _, More _ -> -1
    | More _, Done _ -> 1
    | Done c1, Done c2 -> Int.compare c1.clause c2.clause
    | More (t1, k1'), More (t2, k2') ->
      let c = RE.compare t1 t2 in
      if c <> 0 then c else
        compare k1' k2'
end

module KRESet = struct
  include Set.Make(KRE)

  let prederive ~visited ~reached ~direct ~reduce k =
    let rec loop k =
      if not (mem k !visited) then (
        visited := add k !visited;
        match k with
        | Done {clause} -> push reached clause
        | More (re, k') ->
          match re.desc with
          | Set (s, var) ->
            push direct (s, Option.to_list var, k')
          | Alt es ->
            List.iter (fun e -> loop (KRE.more e k')) es
          | Star r ->
            loop k';
            loop (More (r, k))
          | Seq es ->
            loop (List.fold_right KRE.more es k')
          | Reduce ->
            push reduce k';
            loop k'
      )
    in
    loop k

  let derive_reduce t : t dfa_transition list =
    let visited = ref empty in
    let direct = ref [] in
    let push_k k = push direct (Lr1.all, [], k) in
    let loop k =
      match k with
      | KRE.Done _ -> push_k k
      | k ->
        let reached = ref [] in
        prederive ~visited ~direct ~reached ~reduce:(ref []) k;
        List.iter (fun clause -> push_k (KRE.Done {clause})) !reached
    in
    iter loop t;
    dfa_normalize_and_merge ~compare:KRE.compare ~merge:of_list
      (List.map (fun (s, _v, k) -> (s, k)) !direct)

  let cmon t = Cmon.list_map KRE.cmon (elements t)
end
