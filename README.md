# cstring - OCaml C String Library

A pure OCaml implementation of standard C string functions.

## Features 

- Full implementation of C string.h functions
- Preserves null termination behavior
- Pure OCaml, no dependencies
- Single header style
- Drop-in replacement for C string functions

## Usage

Just copy `cstring.ml` into your project:

```ocaml
open Cstring

let () = 
 let s = "Hello\000World" in
 Printf.printf "Length: %d\n" (strlen s);  (* // prints 5 *)
 Printf.printf "Found: %b\n" (strchr s (Char.code 'o') <> None)  (* // prints true *)