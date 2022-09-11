(* open Printf *)
open Base



let digitCount num =
  match num with
  | x when x >= 100_000_000 && x <= 999_999_999 -> Some 9
  | x when x >= 10_000_000 && x <= 99_999_999 -> Some 8
  | x when x >= 1_000_000 && x <= 9_999_999 -> Some 7
  | x when x >= 100_000 && x <= 999_999 -> Some 6
  | x when x >= 10_000 && x <= 99_999 -> Some 5
  | x when x >= 1000 && x <= 9999 -> Some 4
  | x when x >= 100 && x <= 999 -> Some 3
  | x when x >= 10 && x <= 99 -> Some 2
  | x when x >= 0 && x <= 9 -> Some 1
  | _ -> None


let getDigit num idx =
  let digCount = digitCount num in
  match digCount with
  | None -> None
  | Some count ->
     if idx > count then None
     else Some ((num % (Int.pow 10 idx)) / (Int.pow 10 (idx - 1)))

let isNumberPalindrome num =
  let digCount = digitCount num in
  
  match digCount with
  | None -> false
  | Some count -> begin

  let i = ref 1 in
  let j = ref count in
  let isPalindrome = ref true in 
  
  while !i < !j && !isPalindrome do

    let front = getDigit num !i in
    let back = getDigit num !j in

    match front, back with
    | None, _ -> isPalindrome := false;
    | _, None -> isPalindrome := false;
    | Some x, Some y -> if x <> y then isPalindrome := false;
       
    
    Int.incr i;
    Int.decr j;
    
  done; !isPalindrome
  end 
(*   let str = Int.to_string num in *)
  

let sumFib x =
 
  let term1 = ref 1 in
  let term2 = ref 2 in
  let sum = ref 0 in 
  let next = ref 0 in

  sum := !term2;
  while !term1 < x && !term2 < x do

    next := !term1 + !term2;

    if !next % 2 = 0 then
      sum := !sum + !next
    else ();

    if !term1 < !term2 then
      term1 := !next
    else
      term2 := !next;

    (* printf "%d %d\n" !term1 !term2 *)

    (* printf "next %d" !next; *)
  done; !sum
  


let calcPrimes limit =
  let primeList = [2; 3; 5] in
  let rec calcPrimes' list primeList' max el =
    (* List.iter (fun x -> printf "%d " x) list; *)
    (* (printf "%d,  %d\n" max el);  *)
      match list with
    | [] -> let sorted = (el::primeList') in calcPrimes'  sorted sorted max (el + 1)
    | h::tail ->
    if el >= max then primeList'  
    else if (el % h) = 0 then let sorted = primeList' in calcPrimes' sorted sorted  max (el + 1)
    else calcPrimes' tail primeList' max el
  in calcPrimes' primeList primeList limit 7 


let calcPrimesImp max =
  let q = Queue.create ~capacity:1024 () in
  Queue.enqueue q 2;
  Queue.enqueue q 3;
  Queue.enqueue q 5;

  let el = ref 7 in
  let isPrime = ref true in
  let idx = ref 0 in

  while !el <= max do

    isPrime := true;
    idx := 0;

    while !isPrime && !idx < (Queue.length q) do
      if !el % (Queue.get q !idx) = 0 then isPrime := false else ();
      idx := !idx + 1;
    done;

    if !isPrime then (Queue.enqueue q !el) else ();

    el := !el + 1;
  done; q

let pythTriplets a b c =
  a * a + b * b = c * c

let generateTriangNumbers maxIndex =
  let q = Queue.create ~capacity:maxIndex () in

  Queue.enqueue q 1;
  for i = 1 to maxIndex - 1 do
    let prev = Queue.get q (i - 1) in
    Queue.enqueue q (prev + i + 1);
  done; q

let countDivisors num =
  let rec countDivisors' count idx num =
    if idx >= num then count
    else if num % idx = 0 then countDivisors' (count + 1) (idx + 1) num
    else countDivisors' count (idx + 1) num
  in
  countDivisors' 0 1 num


let findOrCreateSet table key =
  match Hashtbl.find table key with
  | None -> Hash_set.create (module Int) ~growth_allowed:true ~size:10
  | Some set -> set


let computeDiv num =
  let set = Hash_set.create (module Int) ~growth_allowed:true ~size:10
  in
  for i = 1 to num / 2 do
    if num % i = 0 then
      Hash_set.add set i
    else ();
  done; set

let countDivisorsImp table num =
  (* let count = ref 0 in *)
  for i = 1 to num / 2 do
    if num % i = 0 then
      let myset = findOrCreateSet table num in
      Hash_set.add myset num;
      if Hashtbl.mem table i then
        let set3 = Hash_set.union myset (Hashtbl.find_exn table i) in
        Hashtbl.set table ~key:num ~data:set3;
      else

        let s = computeDiv i in
        let myset = Hash_set.union s myset in
        Hash_set.add myset i;
        Hashtbl.set table ~key:i ~data:s;
        Hashtbl.set table ~key:num ~data:myset;
      
      (* Int.incr count *)
    else ();
  done; table 
  
let computeDivisors table num =
  (* let table = Hashtbl.create (module Int) ~size:num ~growth_allowed:true in *)

  for i = 1 to num / 2 do
    for j = 1 to num do
      let set = findOrCreateSet table j in
      if j % i = 0 then
        let seti = findOrCreateSet table i in
        let setj = Hash_set.union seti set in
        Hash_set.add setj j;
        Hashtbl.set table ~key:j ~data:setj;
      else ();
    done;
  done; table
