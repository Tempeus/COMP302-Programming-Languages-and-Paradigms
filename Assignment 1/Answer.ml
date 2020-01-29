(* Q1 TODO: Correct these tests for the double function. *)
let double_tests = [
  (0, 0);
  (1, 2);
  (3, 6);
]

(* Q1 TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec double n = match n with
  | 0 -> 0
  | n -> 2 + double (n - 1)


(* Q1 TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
let fact_tests = [
  (* Your test cases go here.
     Remember that the outputs of fact should be *floating-point* numbers.
  *)
  (0,1.0);
  (1,1.0);
  (2,2.0);
  (3,6.0);
  (4,24.0);
  (5,120.0);
  (6,720.0);
  (7,5040.0);
]

(* Q1 TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float = match n with
  | 0 -> 1.0
  | _ -> float_of_int(n) *. fact(n-1)

(* Q2 TODO: Write your own tests for the mysqrt function.
         You should NOT test cases for n < 0.
*)
let mysqrt_tests = [
  (* Your test cases go here. *)
  (0.0,0.0);
  (1.0,1.0);
  (4.0,2.0);
  (9.0,3.0);
  (16.0,4.0);
  (25.0,5.0);
  (36.0,6.0);
  (49.0,7.0);
  (64.0,8.0);
  (81.0,9.0);
  (100.0,10.0);
]

(* Q2 TODO: Implement mysqrt. *)
let mysqrt (x:float) = 
  let rec helper x g = 
    if close(x,square(g)) then g
    else helper x ((g +. (x /. g))/. 2.0)
  in
  helper x 1.0

(* Q3 TODO: Write your own tests for the cube_root function.
            You should NOT test cases for n < 0.
*)
let cube_root_tests = [
  (* Your test cases go here. *)
  (0.0,0.0);
  (1.0,1.0); 
  (8.0,2.0);
  (27.0,3.0);
  (64.0,4.0);
  (125.0,5.0);
  (216.0,6.0);
]

(* Q3 TODO: Implement cube_root. *)
let cube_root (x:float) =
  let rec helper x g =
    if close(x,cube(g)) then g
    else helper x ((2.0 *.g +. (x /. square(g)))/. 3.0)
  in
  helper x 1.0
(* Q4 TODO: Write your own tests for the fast_exp function.
            You should NOT test cases for negative bases or powers.
*)
let fast_exp_tests = [
  (* Your test cases go here. *) 
  ((0,1),0);
  ((1,2),1);
  ((2,2),4);
  ((10,2),100);
  ((2,3),8);
  ((6,4),1296);
  ((1,1),1);
  ((1,0),1); 
]

(* Q4 TODO: Implement tail recursive helper fast_exp_aux. *)
let rec fast_exp_aux (base, power, acc) =
  if power = 0 then acc
  else if (odd power) then
    fast_exp_aux (base, power - 1, acc * base)
  else 
    fast_exp_aux(base*base, power / 2, acc) 
      
(* Q4 TODO: Implement fast_exp using fast_exp_aux. *) 
let fast_exp (base, power) = 
  if base = 0 then 0
  else fast_exp_aux (base, power, 1)
                                         

                           

