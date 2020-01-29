(* Question 1. *)

let common_tests = [
  (([1;3;2;4;1;5;6;3] , [3;8;8;2;11;21;3]) , [3;2]);
  (([] , []), []);
  (([], [1;2;3;4]) , []);
  (([1;3;5;7;9], [2;4;6;8;10]) , []);
]

let rec common twolists =
  match twolists with 
  |([],_) -> []
  |(_,[]) -> []
  |(x::xs, ys) -> if List.mem x ys then x :: common (xs, remove(x,ys))
      else common(xs, ys)
;;

(* Question 2. Mergesort requires that you use recursion.  Using List.sort or
some other sort defeats the whole purpose.  This question is for the
implementation of split.*)

let split_tests = [
  ([],([],[]));
  ([1],([1],[]));
  ([5; 3; 8],([5; 8],[3]));
  ([7; 4; 7; 1],([7; 7],[4; 1]));
]

let rec split l =
  match l with
  |[] -> ([],[])
  |[x] -> ([x],[])
  |x::y::xs -> let (xx,yy) = split xs
      in (x::xx, y::yy)
;;

(* Question 3 Here you implement merge. *)

let merge_tests = [
  (([],[]),[]);
  (([1;3;6],[]),[1;3;6]);
  (([],[5;7;9]),[5;7;9]);
  (([1],[2;3;5]),[1;2;3;5]);
  (([1;3],[2]),[1;2;3]);
  (([1;3;5],[2;4;6]),[1;2;3;4;5;6]); 
]

let rec merge twolists =
  match twolists with
  |([],[]) -> []
  |(xs,[]) -> xs
  |([],ys) -> ys
  |(x::xs,y::ys) -> if x < y then x :: merge(xs,y::ys)
      else y :: merge(x::xs, ys)
;;

(* Question 4 Finally you combine split and merge and use them to implement mergesort. *)

let mergesort_tests = [
  ([],[]);
  ([1],[1]); 
  ([-5;-3;-2;-1],[-5; -3; -2; -1]);
  ([5;3;7;1],[1;3;5;7])
]

let rec mergesort l =
  match l with
  |[] -> l
  |[_] -> l
  |xs -> let (right, left) = split xs in
      merge (mergesort right, mergesort left)
;;
