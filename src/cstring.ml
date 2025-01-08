let ones = lnot 0 / 0xFF
let highs = ones * 0x80

let has_zero x = ((x - ones) land (lnot x) land highs) <> 0

let to_lower c = Char.chr ((Char.code c) lor 0x20)

let rec strlen_aux s i = 
   match i < String.length s with
   | false -> i
   | true -> match s.[i] with
      | '\000' -> i
      | _ -> strlen_aux s (i + 1)

let strlen s = strlen_aux s 0

let rec strnlen_aux s i max = 
   match i, max with
   | i, 0 -> i
   | i, _ when i >= String.length s -> i
   | i, max -> match s.[i] with
      | '\000' -> i
      | _ -> strnlen_aux s (i + 1) (max - 1)

let strnlen s max = strnlen_aux s 0 max

let rec strcpy_aux dst src i =
   match i < String.length src with
   | false -> dst
   | true -> match src.[i] with
      | '\000' -> dst
      | c -> 
         let _ = Bytes.set dst i c in 
         strcpy_aux dst src (i + 1)

let strcpy src =
   let len = strlen src in
   let dst = Bytes.create len in
   Bytes.unsafe_to_string (strcpy_aux dst src 0)

let rec fill_zeros dst start n =
   match n with
   | 0 -> dst
   | n -> 
      let _ = Bytes.set dst start '\000' in
      fill_zeros dst (start + 1) (n - 1)

let rec strncpy_aux dst src i n = 
   match i, n with
   | _, 0 -> dst
   | i, n when i >= String.length src -> fill_zeros dst i n
   | i, n -> 
      let _ = Bytes.set dst i src.[i] in
      strncpy_aux dst src (i + 1) (n - 1)

let strncpy src n =
   let dst = Bytes.create n in
   Bytes.unsafe_to_string (strncpy_aux dst src 0 n)

let rec strcmp_aux s1 s2 i = 
   let get_char s i = 
      match i >= String.length s with
      | true -> '\000'
      | false -> s.[i]
   in
   match get_char s1 i, get_char s2 i with
   | '\000', '\000' -> 0
   | c1, c2 when c1 = c2 -> strcmp_aux s1 s2 (i + 1)
   | c1, c2 -> Char.code c1 - Char.code c2

let strcmp s1 s2 = strcmp_aux s1 s2 0

let rec strncmp_aux s1 s2 i n = 
   match i, n with
   | _, 0 -> 0
   | i, n -> 
      let c1 = match i >= String.length s1 with
         | true -> '\000'
         | false -> s1.[i]
      in
      let c2 = match i >= String.length s2 with
         | true -> '\000'
         | false -> s2.[i]
      in
      match c1, c2 with
      | '\000', '\000' -> 0
      | c1, c2 when c1 = c2 -> strncmp_aux s1 s2 (i + 1) (n - 1)
      | c1, c2 -> Char.code c1 - Char.code c2

let strncmp s1 s2 n = strncmp_aux s1 s2 0 n

let rec strcasecmp_aux s1 s2 i = 
   let get_char s i = 
      match i >= String.length s with
      | true -> '\000'
      | false -> to_lower s.[i]
   in
   match get_char s1 i, get_char s2 i with
   | '\000', '\000' -> 0
   | c1, c2 when c1 = c2 -> strcasecmp_aux s1 s2 (i + 1)
   | c1, c2 -> Char.code c1 - Char.code c2

let strcasecmp s1 s2 = strcasecmp_aux s1 s2 0

let rec strncasecmp_aux s1 s2 i n = 
   match i, n with
   | _, 0 -> 0
   | i, n -> 
      let c1 = match i >= String.length s1 with
         | true -> '\000'
         | false -> to_lower s1.[i]
      in
      let c2 = match i >= String.length s2 with
         | true -> '\000'
         | false -> to_lower s2.[i]
      in
      match c1, c2 with
      | '\000', '\000' -> 0
      | c1, c2 when c1 = c2 -> strncasecmp_aux s1 s2 (i + 1) (n - 1)
      | c1, c2 -> Char.code c1 - Char.code c2

let strncasecmp s1 s2 n = strncasecmp_aux s1 s2 0 n

let rec strchr_aux s c i = 
   match i < String.length s with
   | false -> if c = '\000' then Some "" else None
   | true -> match s.[i] with
      | c' when c' = c -> Some (String.sub s i (String.length s - i))
      | '\000' -> if c = '\000' then Some "" else None
      | _ -> strchr_aux s c (i + 1)
      

let strchr s c = strchr_aux s (Char.chr (c land 0xFF)) 0

let rec strrchr_aux s c i last = 
   match i < String.length s with
   | true -> (match s.[i] with
      | '\000' -> strrchr_aux s c (i + 1) last
      | c' when c' = c -> strrchr_aux s c (i + 1) (Some i)
      | _ -> strrchr_aux s c (i + 1) last)
   | false -> (match last with
      | Some pos -> Some (String.sub s pos (String.length s - pos))
      | None -> if c = '\000' then Some "" else None)

let strrchr s c = strrchr_aux s (Char.chr (c land 0xFF)) 0 None

let rec check_match haystack needle i j =
   match j with
   | j when j >= String.length needle -> true
   | j when i + j >= String.length haystack -> false
   | j -> match haystack.[i + j] = needle.[j] with
      | false -> false
      | true -> check_match haystack needle i (j + 1)

let rec strstr_aux haystack needle i =
   match String.length needle with
   | 0 -> Some haystack
   | _ -> match i >= String.length haystack with
      | true -> None
      | false -> match haystack.[i] with
         | '\000' -> None
         | _ -> match check_match haystack needle i 0 with
            | true -> Some (String.sub haystack i (String.length haystack - i))
            | false -> strstr_aux haystack needle (i + 1)

let strstr haystack needle = strstr_aux haystack needle 0

let rec copy_string result start s len i =
   match i with
   | i when i >= len -> ()
   | i -> 
      let _ = Bytes.set result (start + i) s.[i] in
      copy_string result start s len (i + 1)

let strcat dst src =
   let dst_len = strlen dst in
   let src_len = strlen src in
   let result = Bytes.create (dst_len + src_len + 1) in
   let _ = copy_string result 0 dst dst_len 0 in
   let _ = copy_string result dst_len src src_len 0 in
   let _ = Bytes.set result (dst_len + src_len) '\000' in
   Bytes.unsafe_to_string result

let rec strncat_aux dst src i n = 
   match i, n with
   | _, 0 -> dst
   | i, n -> match src.[i] with
      | '\000' -> dst
      | c -> 
         let _ = Bytes.set dst i c in 
         strncat_aux dst src (i + 1) (n - 1)

let strncat dst src n =
   let dst_len = strlen dst in
   let src_len = min (strlen src) n in
   let result = Bytes.create (dst_len + src_len + 1) in
   let _ = copy_string result 0 dst dst_len 0 in
   let _ = copy_string result dst_len src src_len 0 in
   let _ = Bytes.set result (dst_len + src_len) '\000' in
   Bytes.unsafe_to_string result

let rec strlcat_aux dst src dst_len maxlen i =
   match maxlen - dst_len, i with
   | 0, _ -> dst_len + strlen src
   | n, i -> match i >= String.length src with
      | true -> dst_len + i
      | false -> match src.[i] with
         | '\000' -> dst_len + i
         | c -> 
            let _ = Bytes.set dst (dst_len + i) c in
            strlcat_aux dst src dst_len maxlen (i + 1)

let strlcat dst src maxlen =
   let dst_len = min (strlen dst) maxlen in
   let result = Bytes.create maxlen in
   let _ = copy_string result 0 dst dst_len 0 in
   strlcat_aux result src dst_len maxlen 0

let rec strnchr_aux s i count c = 
   match i, count with
   | _, 0 -> None
   | i, _ when i >= String.length s -> None
   | i, count -> match s.[i] with
      | '\000' -> None
      | ch when ch = c -> Some (String.sub s i (min count (String.length s - i)))
      | _ -> strnchr_aux s (i + 1) (count - 1) c

let strnchr s count c = strnchr_aux s 0 count (Char.chr c)

let rec strnstr_check_match s1 s2 i l2 =
   match l2 with
   | j when j <= 0 -> true
   | j -> match s1.[i + j - 1] = s2.[j - 1] with
      | false -> false
      | true -> strnstr_check_match s1 s2 i (j - 1)

let rec strnstr_aux s1 s2 len i =
   let l2 = strlen s2 in
   match l2, len with
   | 0, _ -> Some s1
   | l2, len when l2 > len -> None
   | l2, len when i > len - l2 -> None
   | l2, _ -> match strnstr_check_match s1 s2 i l2 with
      | true -> Some (String.sub s1 i (String.length s1 - i))
      | false -> strnstr_aux s1 s2 len (i + 1)

let strnstr s1 s2 len = strnstr_aux s1 s2 len 0