(** Pure OCaml implementation of optimized C string functions, with null termination behavior *)

(** Fast check for zero bytes in a word *)
val has_zero : int -> bool

(** Length of null-terminated string *)
val strlen : string -> int

(** Length with maximum bound *)
val strnlen : string -> int -> int

(** String copy with null termination *)
val strcpy : string -> string

(** Bounded string copy *)
val strncpy : string -> int -> string

(** String concatenation *)
val strcat : string -> string -> string

(** Bounded string concatenation *)
val strncat : string -> string -> int -> string

(** Size-bounded string concatenation *)
val strlcat : string -> string -> int -> int

(** String comparison *)
val strcmp : string -> string -> int

(** Bounded string comparison *)
val strncmp : string -> string -> int -> int

(** Case-insensitive string comparison *)  
val strcasecmp : string -> string -> int

(** Bounded case-insensitive comparison *)
val strncasecmp : string -> string -> int -> int

(** Find first occurrence of character *)
val strchr : string -> int -> string option

(** Find last occurrence of character *)
val strrchr : string -> int -> string option

(** Find character with bound *)
val strnchr : string -> int -> int -> string option

(** Find substring with bound *)
val strnstr : string -> string -> int -> string option

(** Find substring *)
val strstr : string -> string -> string option