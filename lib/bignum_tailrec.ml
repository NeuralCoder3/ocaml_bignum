open Bignum
;;

module BigNumTail : BigNum = struct

  type t = bool list

  let bit2int b = if b then 1 else 0

  (* only works for small numbers *)
  let rec bignum_of_int (n:int) = 
    if n < 0 then failwith "bignum_of_int: negative number" else
    if n = 0 then []
    else (n mod 2 = 1) :: bignum_of_int (n / 2)

  let rec int_of_bignum b : int =
    match b with
    | [] -> 0
    | h::t -> (bit2int h) + 2 * int_of_bignum t

  let rec is_zero b =
    match b with
    | [] -> true
    | h::t -> not h && is_zero t

  let bignum_of_string ?(base=2) s : t =
    if base <> 2 then failwith "bignum_of_string: only base 2 supported" else
    let len = String.length s in
    let rec aux s i =
      if i = len then []
      else
        (s.[i] = '1') :: aux s (i + 1)
    in
    aux s 0 |> List.rev
  
  let append_tail xs ys =
    let rec aux xs ys =
      match ys with
      | [] -> xs
      | h::t -> aux (h::xs) t
    in
    aux ys (List.rev xs)

  let add b1 b2 =
    let rec aux b1 b2 (carry:bool) num =
      match b1, b2, carry with
      | [], b2, false -> append_tail (List.rev num) b2
      | [], b2, true -> aux [true] b2 false num
      | b1, [], false -> append_tail (List.rev num) b1
      | b1, [], true -> aux b1 [true] false num
      | h1::t1, h2::t2, c -> 
        let sum = bit2int h1 + bit2int h2 + bit2int c in
         aux t1 t2 (sum > 1) ((sum mod 2 = 1)::num)
    in
    aux b1 b2 false []

  let sub b1 b2 =
    let rec aux b1 b2 (carry:bool) num =
      match b1, b2,carry with
      | [], _, c -> 
        if c || not(is_zero b2) then [] else List.rev num
      | _, [], false -> 
          append_tail (List.rev num) b1
      | _, [], true -> aux b1 [true] false num
      | h1::t1, h2::t2, c -> 
        let diff = (bit2int h1) - (bit2int h2) - (bit2int c) in
        aux t1 t2 (diff < 0) (((abs diff) mod 2 = 1) :: num)
    in
    aux b1 b2 false []

  let mul b1 b2 =
    (* cps -> larger overflow limit but still there *)
    (* let rec aux b1 b2 cont =
      match b1 with
      | [] -> cont []
      | false::t1 -> aux t1 b2 (fun acc -> cont (false::acc))
      | true::t1 -> aux t1 b2 (fun acc -> cont (add b2 (false::acc)))
    in
    aux b1 b2 Fun.id *)
    let rec aux b1 b2 acc =
      match b1 with
      | [] -> acc
      | false::t1 -> aux t1 b2 (false::acc)
      | true::t1 ->  aux t1 b2 (add b2 (false::acc))
    in
    aux (List.rev b1) b2 []

  let cmp b1 b2 =
    let rec aux b1 b2 c =
    match b1, b2 with
    | [], [] -> c
    | [], _ -> if is_zero b2 then c else Lt
    | _, [] -> if is_zero b1 then c else Gt
    | h1::t1, h2::t2 -> 
      aux t1 t2 (if h1 = h2 then c else if h1 then Gt else Lt)
    in 
    aux b1 b2 Eq

  (* long division *)
  let divmod b1 b2 =
    if is_zero b2 then failwith "divmod: division by zero" else
    let n = List.rev b1 in
    let d = b2 in
    let rec aux n r q =
      match n with
      | [] -> (q,r)
      | n0::nr ->
        let r' = n0::r in
        if cmp r' d = Lt then 
          aux nr r' (false::q)
        else 
          aux nr (sub r' d) (true::q)
    in
    aux n [] [] 

  let modulo b1 b2 = snd (divmod b1 b2)
  let div b1 b2 = fst (divmod b1 b2)

  let string_of_bignum ?(base=2) b : string =
    if is_zero b then "0" else
    let rec aux b s = 
      if is_zero b then s
      else
        let (q, r) = divmod b (bignum_of_int base) in
        aux q (string_of_int (int_of_bignum r) ^ s)
    in
    aux b ""

  let shr b1 b2 = 
    let rec aux b1 n =
      if n = 0 then b1
      else
        match b1 with
        | [] -> []
        | _::t -> aux t (n - 1)
    in
    aux b1 (int_of_bignum b2)

  let shl b1 b2 =
    let rec aux b1 n =
      if n = 0 then b1
      else aux (false::b1) (n - 1)
    in
    aux b1 (int_of_bignum b2)

  let abs = Fun.id

  let pow b1 b2 =
    let rec aux b1 b2 a =
      if is_zero b2 then a
      else 
        match b2 with
        | [] -> assert false
        | false::t -> 
          aux (mul b1 b1) t a
        | true::t ->
          aux (mul b1 b1) t (mul b1 a)
    in
    aux b1 b2 (bignum_of_int 1)


end


module BigNumTailN : BigNum = struct

  let base = 16
  type t = int list

  (* only works for small numbers *)
  let rec bignum_of_int (n:int) = 
    if n < 0 then failwith "bignum_of_int: negative number" else
    if n = 0 then []
    else (n mod base) :: bignum_of_int (n / base)

  let rec int_of_bignum b : int =
    match b with
    | [] -> 0
    | h::t -> h + base * int_of_bignum t

  let rec is_zero b =
    match b with
    | [] -> true
    | h::t -> h=0 && is_zero t

  let digitToChar n =
    if n < 10 then char_of_int (n + int_of_char '0')
    else char_of_int (n - 10 + int_of_char 'a')

  let charToDigit c =
    if c >= '0' && c <= '9' then int_of_char c - int_of_char '0'
    else if c >= 'a' && c <= 'f' then int_of_char c - int_of_char 'a' + 10
    else failwith "charToDigit: not a valid hex digit"

  
  let append_tail xs ys =
    let rec aux xs ys =
      match ys with
      | [] -> xs
      | h::t -> aux (h::xs) t
    in
    aux ys (List.rev xs)

  let add b1 b2 =
    let rec aux b1 b2 (carry:int) num =
      match b1, b2 with
      | [], b2 when carry=0 -> append_tail (List.rev num) b2
      | [], b2 -> aux [carry] b2 0 num
      | b1, [] when carry=0 -> append_tail (List.rev num) b1
      | b1, [] -> aux b1 [carry] 0 num
      | h1::t1, h2::t2 -> 
        let sum = h1 + h2 + carry in
         aux t1 t2 (sum / base) ((sum mod base)::num)
    in
    aux b1 b2 0 []

  let sub b1 b2 =
    let rec aux b1 b2 (carry:int) num =
      match b1, b2 with
      | [], _ -> 
        if carry > 0 || not(is_zero b2) then [] else List.rev num
      | _, [] when carry=0 -> 
        append_tail (List.rev num) b1
      | _, [] -> aux b1 [carry] 0 num
      | h1::t1, h2::t2 -> 
        (* long division *)
        let diff = h1 - h2 - carry in
        aux t1 t2 (if diff < 0 then 1 else 0) (((base+diff) mod base) :: num)
    in
    aux b1 b2 0 []

  let cmp b1 b2 =
    let rec aux b1 b2 c =
    match b1, b2 with
    | [], [] -> c
    | [], _ -> if is_zero b2 then c else Lt
    | _, [] -> if is_zero b1 then c else Gt
    | h1::t1, h2::t2 -> 
      aux t1 t2 (if h1 = h2 then c else if h1>h2 then Gt else Lt)
    in 
    aux b1 b2 Eq

  let shl _ _ = failwith "shl: not implemented"
  let shr _ _ = failwith "shr: not implemented"

  let naiveDivMod b1 b2 : (int*t) =
    let rec aux b1 b2 c =
      if cmp b1 b2 = Lt then (c,b1)
      else aux (sub b1 b2) b2 (c+1)
    in
    aux b1 b2 0

  let divmod b1 b2 =
    if is_zero b2 then failwith "divmod: division by zero" else
    let n = List.rev b1 in
    let d = b2 in
    let rec aux n r q =
      match n with
      | [] -> (q,r)
      | n0::nr ->
        let r' = n0::r in
        let (c,r'') = naiveDivMod r' d in
        aux nr r'' (c::q)
    in
    aux n [] [] 

  let naiveMul c b : t =
    let rec aux c b acc =
      if c = 0 then acc
      else aux (c-1) b (add b acc)
    in
    aux c b []

  let mul b1 b2 =
    (* cps -> larger overflow limit but still there *)
    let rec aux b1 b2 acc =
      match b1 with
      | [] -> acc
      | c::t1 -> aux t1 b2 (add (naiveMul c b2) (0::acc))
    in
    aux (List.rev b1) b2 []

  let pow b1 b2 =
    let rec aux b1 b2 a =
      if is_zero b2 then a
      else 
        let (q,r) = divmod b2 (bignum_of_int 2) in
        if is_zero r then
          aux (mul b1 b1) q a
        else
          aux (mul b1 b1) q (mul b1 a)
    in
    aux b1 b2 (bignum_of_int 1) 

  let abs = Fun.id

  let modulo b1 b2 = snd (divmod b1 b2)
  let div b1 b2 = fst (divmod b1 b2)

  let string_of_bignum ?(base=2) b : string =
    if is_zero b then "0" else
    let rec aux b s = 
      if is_zero b then s
      else
        let (q, r) = divmod b (bignum_of_int base) in
        aux q (String.make 1 (digitToChar (int_of_bignum r)) ^ s)
    in
    aux b ""

  let bignum_of_string ?(base=2) s : t =
    let len = String.length s in
    let bbase = bignum_of_int base in
    let rec aux n s i =
      if i = len then n
      else
        let d = charToDigit s.[i] in
        let b = bignum_of_int d in
        (* add (mul b bbase) (aux s (i + 1)) *)
        aux (add (mul n bbase) b) s (i + 1)
    in
    aux (bignum_of_int 0) s 0

end