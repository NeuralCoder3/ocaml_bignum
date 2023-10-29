open Bignum.BigNum

let () =
  let a1 = 13 in
  let a2 = 7 in
  let b1 = bignum_of_int a1 in
  let b2 = bignum_of_int a2 in
  let b3 = add b1 b2 in
  let b4 = mul b1 b2 in
  let b5 = sub b1 b2 in
  let b6 = sub b2 b1 in
  let b7 = modulo b1 b2 in
  let b8 = div b1 b2 in
  print_endline (int_of_bignum b1 |> string_of_int);
  print_endline (int_of_bignum b2 |> string_of_int);
  print_endline (string_of_bignum b1);
  print_endline (string_of_bignum b2);
  print_endline ("+");
  print_endline (int_of_bignum b3 |> string_of_int);
  print_endline ((a1 + a2) |> string_of_int);
  print_endline ("*");
  print_endline (int_of_bignum b4 |> string_of_int);
  print_endline ((a1 * a2) |> string_of_int);
  print_endline ("-");
  print_endline (int_of_bignum b5 |> string_of_int);
  print_endline ((a1 - a2) |> string_of_int);
  print_endline ("-");
  print_endline (int_of_bignum b6 |> string_of_int);
  print_endline ((a2 - a1) |> string_of_int);
  print_endline ("mod");
  print_endline (int_of_bignum b7 |> string_of_int);
  print_endline ((a1 mod a2) |> string_of_int);
  print_endline ("div");
  print_endline (int_of_bignum b8 |> string_of_int);
  print_endline ((a1 / a2) |> string_of_int); 
  ()