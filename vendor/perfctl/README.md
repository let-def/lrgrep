# perfctl

A wrapper and library to control 'perf' from the program being profiled.

## Overview

`perfctl` is an OCaml library and binary that wraps the `perf` command-line tool to allow the program being profiled to control when profiling starts and stops. This enables precise profiling of specific code sections without manual intervention.

## Features

- Dynamically enable/disable profiling from within the program
- Block until profiling state changes are confirmed
- Lightweight wrapper around Linux `perf`
- Works with OCaml programs (can be adapted for other languages)

## Installation

Install from OPAM:
```bash
opam install perfctl
```

## Usage

### In your OCaml code:

```ocaml

(* Is perfctl available in this environment? *)
let available = Perfctl.is_available

(* Enable profiling (blocks until ready) *)
let () = Perfctl.enable ()

(* Your code to profile here *)

(* Disable profiling (blocks until complete) *)
let () = Perfctl.disable ()
```

Then run your program with `perfctl record -- ./myprogram.exe`

Example test program:

```ocaml
(* Profile multiple sections by calling enable/disable multiple times *)
let () = not_in_profile 1_000_000

let () = Perfctl.enable ()
let () = in_profile 1_000_000
let () = Perfctl.disable ()

(* Code not profiled *)
let () = not_in_profile 1_000_000

(* Profile another section *)
let () = Perfctl.enable ()
let () = in_profile 1_000_000
let () = Perfctl.disable ()

(* Code not profiled *)
let () = not_in_profile 1_000_000
```

## Requirements

- Linux system with `perf` tool installed
- OCaml 4.08 or later

## References

- [Stackoverflow: Perf record after my code reaches a certain point?](https://stackoverflow.com/questions/74340680/perf-record-after-my-code-reaches-a-certain-point)
- [Linux Perf: Measuring Specific Code Sections with Pause/Resume APIs](https://pramodkumbhar.com/2024/04/linux-perf-measuring-specific-code-sections-with-pause-resume-apis/)

## License

MIT
