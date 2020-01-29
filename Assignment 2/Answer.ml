(* Q1a TODO: Write your own tests for the pairlists function.
         You should NOT test lists of different lengths.
*)
let pairlists_tests = [
  (* Your test cases go here. *)
  (([1;2;3],[1;2;3]), [(1,1);(2,2);(3,3)]);
  (([1],[1]), [(1,1)]);
  (([1;2],[3;4]), [(1,3);(2,4)]);
  (([],[]), [])
]

(* Q1a TODO: Implement pairlists. *)
let rec pairlists twolists =
  match twolists with
  | ([],[]) -> []
  | (x::xs), (h::t) -> (x,h)::(pairlists (xs, t))
;;

(* Q1b TODO: Write your own tests for the w_mean function.
         You should NOT test lists of different lengths.
*)
let w_mean_tests = [
  (* Your test cases go here. *)
  (*(([], []), 0.0);*)
  (([1.0], [1.0]), 1.0);
  (([1.0;1.0], [10.0;-10.0]), 0.0);
  (([1.0;1.5;2.0], [1.0;1.5;2.0]), 1.61111111111111116);
  (([1.0; 1.5; 2.5; 0.5; 1.5], [10.3; 11.7; 2.0; 5.0; 6.5]), 6.44285714285714217);
] 

(* Q1b TODO: Implement w_mean. *)
let w_mean weights data = 
  let times (v1, v2) = v1 *. v2 in
  sumlist (List.map times (pairlists (weights, data))) /. sumlist weights
;;

(* Q2 TODO: Write your own tests for the memberof function. *)
let memberof_tests = [
  (* Your test cases go here. *)
  ((1, [1;2;3;4;5;6]), true);
  ((2, []), false);
  ((3, [1;2]), false)
]

(* Q2 TODO: Implement memberof. *)
let rec memberof pair =
  match pair with
  | (x,[]) -> false
  | (x, h::t) -> if (x = h) then true
      else memberof (x, t)
;;

(* Q2 TODO: Write your own tests for the remove function. *)
let remove_tests = [
  (* Your test cases go here. *)
  ((1, [1;2;3;4;5;6]), [2;3;4;5;6]);
  ((2, []), []);
  ((2, [1;3;4]), [1;3;4]);
  ((3, [1;2;3;4;5;6;3]), [1;2;4;5;6])
]

(* Q2 TODO: Implement remove. *)
let rec remove (item, lst) =
  match lst with
  | [] -> []
  | x::xs -> if (x = item) then remove (item, xs)
      else x::(remove (item,xs))
;;

(* Q3 TODO: Write your own tests for the find_max function. *)
let find_max_tests = [
  (* Your test cases go here. *)
  ([1;2;3;4;5], 5);
  ([5;2;3;4;10], 10);
  ([0], 0);
  ([-100;-10], -10);
  ([2;10;5;16;4;3;7], 16)
]

(* Q3 TODO: Implement find_max. *)
let find_max l = 
  let rec maxHelper (l,max)=
    match l with
    | [] -> max
    | x::xs -> if (x > max) then maxHelper (xs, x)
        else maxHelper (xs, max)
  in maxHelper(l, List.hd l)
;;

(* Q4 TODO: Write your own tests for the selsort function. *)
let selsort_tests = [
  (* Your test cases go here. *)
  ([], []);
  ([0], [0]);
  ([1;2;3;4;5], [5;4;3;2;1]);
  ([5;4;3;2;1], [5;4;3;2;1]);
  ([-1;10;0;-51], [10;0;-1;-51])
]

(* Q4 TODO: Implement selsort. *)
let rec selsort l = 
  if (List.length l = 0) then []
  else let max = find_max l in 
    max::(selsort(remove (max, l)))
;;
