open Bignum

let test ?(verbose=None) choose f1 f2 n () =
  let rec aux n =
    if n = 0 then Result.ok ()
    else
      let a = choose () in
      (match verbose with
      | None -> ()
      | Some verbose -> Printf.printf "%s\n%!" (verbose a));
      let r1 = f1 a in
      let r2 = f2 a in
      if r1 = r2 then aux (n - 1)
      else Result.error (a, r1, r2)
  in
  aux n

let eval_result f =
  try 
  match f () with
  | Result.Ok () -> "OK"
  | Result.Error ((a,b), r1, r2) -> Printf.sprintf "ERROR on %d %d: %d <> %d" a b r1 r2
  with e -> Printexc.to_string e

let random_pair ?(bound=1000000) ?(signed=false) () =
  let a = Random.int bound - (if signed then bound / 2 else 0) in
  let b = Random.int bound - (if signed then bound / 2 else 0) in
  (a, b)


let () = Printf.printf "Testing BigInt\n%!"


(* open BigNum *)
(* open (BigInt (BigNum)) *)
open Bignum_tailrec
(* open BigNumTail *)
open BigNumTailN

let test_big_int ?(guard=Fun.id) f g =
  test 
    (* ~verbose:(Some (fun (a, b) -> Printf.sprintf "%d %d" a b)) *)
    (random_pair ~signed:false)
    (* (random_pair ~signed:true) *)
    (fun (a, b) -> f a (guard b)) (fun (a, b) -> int_of_bignum (g (bignum_of_int a) (bignum_of_int (guard b)))) 100000

let zero_guard a = if a = 0 then 1 else a
let () = 
  Printf.printf "id : %s\n%!" (eval_result (test_big_int (fun a _ -> a) (fun a _ -> a)));
  Printf.printf "idS: %s\n%!" (eval_result (test_big_int (fun a _ -> a) (fun a _ -> bignum_of_string (string_of_bignum a))));
  Printf.printf "add: %s\n%!" (eval_result (test_big_int ( + ) add));
  Printf.printf "sub: %s\n%!" (eval_result (test_big_int (fun a b -> max (a-b) 0) sub));
  (* Printf.printf "sub: %s\n%!" (eval_result (test_big_int ( - ) sub)); *)
  Printf.printf "mul: %s\n%!" (eval_result (test_big_int ( * ) mul));
  Printf.printf "cmp: %s\n%!" (eval_result (test random_pair (fun (a, b) -> compare a b) (fun (a, b) -> match cmp (bignum_of_int a) (bignum_of_int b) with Lt -> -1 | Eq -> 0 | Gt -> 1) 100000));
  Printf.printf "div: %s\n%!" (eval_result (test_big_int ~guard:zero_guard (fun a b -> a / b) div));
  Printf.printf "mod: %s\n%!" (eval_result (test_big_int ~guard:zero_guard (fun a b -> a mod b) modulo));
  ()




