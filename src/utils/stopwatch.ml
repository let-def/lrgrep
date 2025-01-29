let output = ref prerr_string

let oprintf fmt =
  Printf.ksprintf !output fmt

type float_ref = {
  mutable value: float;
}

type t = {
  start: float;
  time: float_ref;
  indent: int;
}

let create () =
  let time = Sys.time () in
  {start = time; time = {value = time}; indent = 0}

let reset t =
  t.time.value <- Sys.time ()

let indent oc n =
  for _ = 0 to n - 1 do
    output_char oc ' '
  done

let step t fmt =
  let time' = Sys.time () in
  Printf.ksprintf (fun msg ->
      let delta = ((time' -. t.time.value) *. 1000.0) in
      indent stderr t.indent;
      oprintf "| +%.02fms: %s\n" delta msg;
      t.time.value <- time';
    ) fmt

let enter t fmt =
  let time' = Sys.time () in
  Printf.ksprintf (fun msg ->
      let delta = ((time' -. t.time.value) *. 1000.0)  in
      indent stderr t.indent;
      if delta >= 0.01
      then oprintf "\\ %s (after %.02fms)\n" msg delta
      else oprintf "\\ %s\n" msg;
      t.time.value <- time';
      {time = t.time; start = time'; indent = t.indent + 1}
    ) fmt

let leave t =
  let time' = Sys.time () in
  indent stderr t.indent;
  oprintf "(total: %.02fms)\n" ((time' -. t.start) *. 1000.0);
  t.time.value <- time'

let main = create ()
