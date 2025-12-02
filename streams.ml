type 'a str = Cons of 'a * ('a stream) | Nil
and  'a stream = unit -> 'a str
       
exception Subscript
exception Empty
       
let head (s :'a stream) : 'a =
  match s () with
    Cons (hd,tl) -> hd
  | Nil -> raise Empty
                      
let tail (s :'a stream) : 'a stream =
  match s () with
    Cons (hd,tl) -> tl
  | Nil -> raise Empty
  
let null (s : 'a stream) =
  match s () with
    Nil -> true
  | _ -> false
  
let rec take (n: int) (s: 'a stream) : 'a list = 
  match n with
    n when n > 0 -> head s :: take (n - 1) (tail s)
  | 0 -> []
  | _ -> raise Subscript
  
let rec nth (n: int) (s: 'a stream) : 'a =
  match n with
    n when n > 0 -> nth (n - 1) (tail s)
  | 0 -> head s
  | _ -> raise Subscript


let rec map (f: 'a -> 'b) (s:'a stream) : 'b stream =
  fun () -> Cons (f (head s), map f (tail s))
  
let rec filter (s: 'a stream) (f: 'a -> bool) : 'a stream = 
  if f (head s) 
  then fun () -> Cons (head s, filter (tail s) f)
  else filter (tail s) f


let rec sieve (s: int stream) : int stream =
  fun () -> Cons(head s, sieve (filter (tail s) (fun x -> x mod (head s) <> 0)))


let rec fromn (n: int) = fun () -> Cons (n, fromn (n + 1))
let rec fib n m = fun () -> Cons (n, fib m (n+m))
  
(*even is true iff x is divisible by 2*)
let even : int -> bool =
  fun x -> x mod 2 = 0

(*odd is true iff x is not divisible by 2*)
let odd  : int -> bool =
  fun x -> x mod 2 <> 0

(*squares is the stream 1^2,2^2,3^2,...*)
let squares : int stream =
  map (fun x -> x * x) (fromn 1)

(*fibs is the fibonacci stream 0,1,1,2,3,...*)
let fibs : int stream =
  fib 0 1

(*evenFibs filters fibs using even predicate*)
let evenFibs : int stream =
  filter fibs even

(*oddFibs filters fibs using odd predicate*)
let oddFibs : int stream =
  filter fibs odd

(*primes is the stream of prime numbers via sieve starting at 2*)
let primes : int stream =
  sieve (fromn 2)

(*rev_zip_diff produces (b,a,f(b,a)) by swapping pair order*)
let rev_zip_diff : 'a stream -> 'b stream -> ('b * 'a -> 'c) -> ('b * 'a * 'c) stream =
  fun a b f ->
    let rec aux sa sb =
      fun () ->
        let ba = (head sb, head sa) in
        let c = f ba in
        Cons ((fst ba, snd ba, c), aux (tail sa) (tail sb))
    in
    aux a b

(*printGenList applies f to each element in l*)
let rec printGenList : 'a list -> ('a -> unit) -> unit =
  fun l f ->
    match l with
    | [] -> ()
    | x :: xs -> f x; printGenList xs f
  
(*printList writes ints separated by spaces to file f*)
let rec printList : int list -> string -> unit =
  fun l f ->
    let oc = open_out f in
    (*print each int followed by a space*)
    printGenList l (fun x -> output_string oc (string_of_int x ^ " "));
    close_out oc
  

let rec printPairList : (int * int) list -> string -> unit =
  fun l f ->
    let oc = open_out f in
    (*print each pair in the form (x, y) and a space*)
    printGenList l
      (fun (x,y) ->
         output_string oc ("(" ^ string_of_int x ^ ", " ^ string_of_int y ^ ") "));
    close_out oc
;;


printList [2;3;0;1] "even_odd_test.txt";;
printList (take 10 squares) "squares.txt";;
printList (take 10 fibs) "fibs.txt";;
printList (take 10 evenFibs) "evenFibs.txt";;
printList (take 10 oddFibs) "oddFibs.txt";;
printList (take 10 primes) "primes.txt";;
let rev_zip_pairs = take 5 (rev_zip_diff evenFibs oddFibs (fun (x,y) -> x - y)) in
let pair_list = List.map (fun (a,b,c) -> (a,b)) rev_zip_pairs in
printPairList pair_list "rev_zip_diff.txt";;