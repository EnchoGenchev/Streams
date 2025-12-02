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
  
(* implement the streams and functions below *)

let even : int -> bool = fun x -> true
let odd  : int -> bool = fun x -> true

let squares : int stream = fun () -> Nil
let fibs : int stream = fun () -> Nil
let evenFibs : int stream = fun () -> Nil
let oddFibs : int stream = fun () -> Nil
let primes : int stream = fun () -> Nil

let rev_zip_diff : 'a stream -> 'b stream -> ('b * 'a -> 'c) -> ('b * 'a * 'c) stream =
  fun a b f -> fun () -> Nil

let rec printGenList : 'a list -> ('a -> unit) -> unit =
  fun l f -> ()
  
let rec printList : int list -> string -> unit =
  fun l f ->
    let oc = open_out f in
    close_out oc
  
let rec printPairList : (int * int) list -> string -> unit =
  fun l f ->
    let oc = open_out f in
    close_out oc
