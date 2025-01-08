let ones = Int64.div (Int64.lognot 0L) 0xFFL
let highs = Int64.mul ones 0x80L

let[@inline] has_zero x = Int64.logand (Int64.logand (Int64.sub x ones) (Int64.lognot x)) highs <> 0L

let[@inline] to_lower c = Char.unsafe_chr (Char.code c lor 0x20)

let strlen s =
  let len = String.length s in
  let rec find i =
    if i >= len || String.unsafe_get s i = '\000' then i
    else find (i + 1)
  in find 0

let strnlen s max =
  let len = String.length s in
  let rec find i =
    if i >= len || i >= max || String.unsafe_get s i = '\000' then i
    else find (i + 1)
  in find 0

let[@inline] copy_chunk src src_pos dst dst_pos =
  let s = String.get_int64_ne src src_pos in
  Bytes.set_int64_ne dst dst_pos s

let strcpy src =
  let src_len = strlen src in
  let dst = Bytes.create src_len in
  let rec copy_by_chunk i =
    if i + 8 <= src_len then begin
      copy_chunk src i dst i;
      copy_by_chunk (i + 8)
    end else
      for j = i to src_len - 1 do
        Bytes.unsafe_set dst j (String.unsafe_get src j)
      done
  in
  copy_by_chunk 0;
  Bytes.unsafe_to_string dst

let strncpy src n =
  let src_len = min (strlen src) n in
  let dst = Bytes.create n in
  let rec copy i =
    if i >= n then dst
    else if i >= src_len then begin
      Bytes.unsafe_set dst i '\000';
      copy (i + 1)
    end else begin
      Bytes.unsafe_set dst i (String.unsafe_get src i);
      copy (i + 1)
    end
  in
  Bytes.unsafe_to_string (copy 0)

let strcmp s1 s2 =
  let len1, len2 = String.length s1, String.length s2 in
  let rec compare_chunks i =
    if i + 8 <= min len1 len2 then
      let chunk1 = String.get_int64_ne s1 i in
      let chunk2 = String.get_int64_ne s2 i in
      if chunk1 = chunk2 then compare_chunks (i + 8)
      else Int64.to_int (Int64.sub chunk1 chunk2)
    else compare_bytes i
  and compare_bytes i =
    if i >= len1 then
      if i >= len2 then 0
      else ~-1
    else if i >= len2 then 1
    else
      let c1 = String.unsafe_get s1 i in
      let c2 = String.unsafe_get s2 i in
      if c1 = c2 then compare_bytes (i + 1)
      else Char.code c1 - Char.code c2
  in
  compare_chunks 0

let strncmp s1 s2 n =
  let len1, len2 = min (String.length s1) n, min (String.length s2) n in
  let rec compare i =
    if i >= n then 0
    else if i >= len1 then
      if i >= len2 then 0
      else ~-1
    else if i >= len2 then 1
    else
      let c1 = String.unsafe_get s1 i in
      let c2 = String.unsafe_get s2 i in
      if c1 = c2 then compare (i + 1)
      else Char.code c1 - Char.code c2
  in compare 0

let lowercase_table = Array.init 256 (fun i -> 
  if i >= 65 && i <= 90 then Char.chr (i + 32) else Char.chr i)

let strcasecmp s1 s2 =
  let len1, len2 = String.length s1, String.length s2 in
  let rec compare i =
    if i >= len1 then
      if i >= len2 then 0
      else ~-1
    else if i >= len2 then 1
    else
      let c1 = lowercase_table.(Char.code (String.unsafe_get s1 i)) in
      let c2 = lowercase_table.(Char.code (String.unsafe_get s2 i)) in
      if c1 = c2 then compare (i + 1)
      else Char.code c1 - Char.code c2
  in compare 0

let strncasecmp s1 s2 n =
  let len1, len2 = min (String.length s1) n, min (String.length s2) n in
  let rec compare i =
    if i >= n then 0
    else if i >= len1 then
      if i >= len2 then 0
      else ~-1
    else if i >= len2 then 1
    else
      let c1 = lowercase_table.(Char.code (String.unsafe_get s1 i)) in
      let c2 = lowercase_table.(Char.code (String.unsafe_get s2 i)) in
      if c1 = c2 then compare (i + 1)
      else Char.code c1 - Char.code c2
  in compare 0

let strchr s c =
  let c = Char.chr (c land 0xFF) in
  let len = String.length s in
  let rec find i =
    if i >= len then
      if c = '\000' then Some "" else None
    else
      let curr = String.unsafe_get s i in
      if curr = c then Some (String.sub s i (len - i))
      else if curr = '\000' then
        if c = '\000' then Some "" else None
      else find (i + 1)
  in find 0

let strrchr s c =
  let c = Char.chr (c land 0xFF) in
  let len = String.length s in
  let rec find i last =
    if i >= len then
      match last with
      | Some pos -> Some (String.sub s pos (len - pos))
      | None -> if c = '\000' then Some "" else None
    else
      let curr = String.unsafe_get s i in
      if curr = '\000' then
        match last with
        | Some pos -> Some (String.sub s pos (len - pos))
        | None -> if c = '\000' then Some "" else None
      else if curr = c then
        find (i + 1) (Some i)
      else
        find (i + 1) last
  in find 0 None

let strstr haystack needle =
  if needle = "" then Some haystack else
  let h_len = String.length haystack in
  let n_len = String.length needle in
  if n_len > h_len then None else
  let first = String.unsafe_get needle 0 in
  let last = String.unsafe_get needle (n_len - 1) in
  let rec search i =
    if i > h_len - n_len then None
    else if String.unsafe_get haystack i <> first ||
            String.unsafe_get haystack (i + n_len - 1) <> last then
      search (i + 1)
    else
      let rec check j =
        if j >= n_len - 1 then Some (String.sub haystack i (h_len - i))
        else if String.unsafe_get haystack (i + j) <> String.unsafe_get needle j then
          search (i + 1)
        else check (j + 1)
      in check 1
  in search 0

let strnstr s1 s2 len =
  if s2 = "" then Some s1 else
  let len1 = min (String.length s1) len in
  let len2 = String.length s2 in
  if len2 > len1 then None else
  let first = String.unsafe_get s2 0 in
  let last = String.unsafe_get s2 (len2 - 1) in
  let rec search i =
    if i > len1 - len2 then None
    else if String.unsafe_get s1 i <> first ||
            String.unsafe_get s1 (i + len2 - 1) <> last then
      search (i + 1)
    else
      let rec check j =
        if j >= len2 - 1 then Some (String.sub s1 i (len1 - i))
        else if String.unsafe_get s1 (i + j) <> String.unsafe_get s2 j then
          search (i + 1)
        else check (j + 1)
      in check 1
  in search 0

(* String concatenation operations *)
let[@inline] copy_string dst start src len =
  for i = 0 to len - 1 do
    Bytes.unsafe_set dst (start + i) (String.unsafe_get src i)
  done

let strcat s1 s2 =
  let len1 = strlen s1 in
  let len2 = strlen s2 in
  let result = Bytes.create (len1 + len2) in
  copy_string result 0 s1 len1;
  copy_string result len1 s2 len2;
  Bytes.unsafe_to_string result

let strncat s1 s2 n =
  let len1 = strlen s1 in
  let len2 = min (strlen s2) n in
  let result = Bytes.create (len1 + len2) in
  copy_string result 0 s1 len1;
  copy_string result len1 s2 len2;
  Bytes.unsafe_to_string result

let strlcat dst src maxlen =
  let dst_len = min (strlen dst) maxlen in
  let src_len = strlen src in
  let available = maxlen - dst_len in
  if available <= 0 then dst_len + src_len
  else begin
    let to_copy = min available (src_len + 1) in
    let result = Bytes.create maxlen in
    copy_string result 0 dst dst_len;
    copy_string result dst_len src (to_copy - 1);
    Bytes.unsafe_set result (dst_len + to_copy - 1) '\000';
    dst_len + src_len
  end

let strnchr s count c =
  let c = Char.chr (c land 0xFF) in
  let len = min (String.length s) count in
  let rec find i =
    if i >= len then None
    else
      let curr = String.unsafe_get s i in
      if curr = '\000' then None
      else if curr = c then Some (String.sub s i (len - i))
      else find (i + 1)
  in find 0;;
