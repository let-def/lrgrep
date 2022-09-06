(** A simple preprocessor to embed binary files into an ML file.
    Look at [usage]. *)
let usage =
  "embed [--] value1=<file1> value2=<file2> ...\n\
   prints ocaml code on stdout that looks like:\n\
  \  let value1 = \"<contents of file1>\"\n\
  \  let value2 = \"<contents of file2>\"\n\
  \  ...\n\
  "

let exit_usage () =
  prerr_endline usage;
  exit 1

let process = function
  | "--" -> ()
  | k_v ->
    match String.index_opt k_v '=' with
    | None ->
      prerr_endline ("Invalid argument " ^ k_v);
      exit_usage ()
    | Some pos ->
      let k = String.sub k_v 0 pos in
      let v = String.sub k_v (pos + 1) (String.length k_v - pos - 1) in
      let ic = open_in_bin v in
      let len = in_channel_length ic in
      let contents = really_input_string ic len in
      close_in_noerr ic;
      Printf.printf "let %s = %S\n" k contents

let () =
  let len = Array.length Sys.argv in
  if len = 1 then exit_usage ();
  for i = 1 to len - 1 do
    process Sys.argv.(i)
  done
