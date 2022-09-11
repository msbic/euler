open Printf
open Algos
open Base
(* open Lwt *)


let maxPrimeFactor x =
  let primes = calcPrimesImp 1000000 in
  let qSize = Queue.length primes in

  let idx = ref (qSize - 1) in
  let primeFound = ref false in
  let myPrime = ref 0 in

  while not !primeFound && !idx >= 0 do
    myPrime := Queue.get primes !idx;
    if x % !myPrime = 0 then primeFound := true else ();
    Int.decr idx;
  done; !myPrime
  

(* 600851475143 *)

let findLargestPalindrome () =

  let q = Queue.create () in
  
  for i = 999 downto 800 do
    for j = 999 downto 800 do
      let prod = i * j in
      if isNumberPalindrome prod then Queue.enqueue q prod;
    done;
  done; (Queue.to_array q)
  

let smallestMultiple () =
  let x = ref (2520 * 11 * 13 * 17 * 19) in
  let multipleFound = ref false in
  let ar = Array.init 20 ~f:(fun x -> x + 1) in
  
  while not !multipleFound do
    
    let l = Array.map ar ~f:(fun d -> !x % d) |> Array.filter ~f:(fun y -> y = 0) in 

    if (Array.length l = 20) then multipleFound := true else x := !x + 10;
    
  done; !x


let euler6 () =
  let ar = Array.init 100 ~f:(fun x -> x + 1) in
  let sumOfSqrs = Array.map ar ~f:(fun x -> x * x) |> Array.fold ~init:0 ~f:(+) in
  let sqrOfSum =
    let sum = Array.fold ar ~init:0 ~f:(+) in sum * sum in
  sqrOfSum - sumOfSqrs


let euler6List () =
  let ar = List.init 100 ~f:(fun x -> x + 1) in
  let sumOfSqrs = List.map ar ~f:(fun x -> x * x) |> List.fold ~init:0 ~f:(+) in
  let sqrOfSum =
    let sum = List.fold ar ~init:0 ~f:(+) in sum * sum in
  sqrOfSum - sumOfSqrs


let euler7 () =
  let q = calcPrimesImp 500000 in
  printf "10001 prime is: %d\n" (Queue.get q 10000)


let calcStringProduct str =
  
  let rec calcStringProduct' accum idx str =
    (* printf "idx %d len %d\n" idx (String.length str); *)
    let len = String.length str in

    if idx >= len then accum else
      let digit = (String.get str idx |> Char.to_int) - 48 in
      let accum' = accum * digit in
      (* printf "digit %d accum %d\n" digit accum'; *)
      calcStringProduct' accum' (idx + 1) str

  in calcStringProduct' 1 0 str
    
  

let euler8 () =
  let str = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450" in


  
  let len = String.length str in
  let idx = ref 0 in
  let product = ref 0 in
  let sub = ref "" in
  let p = ref 0 in 

  while !idx + 13 < len do
    let subStr = Stdlib.String.sub str !idx 13 in
    p := calcStringProduct subStr;

    (* Printf.printf "p = %d  sub = %s\n" !p subStr; *)
    
    if (Int.compare !p !product) > 0 then (product := !p; sub := subStr) else ();
    
    idx := !idx + 1;
  done; (!product, !sub)
  

let euler9 () =

  for a = 10 to 1000 do
    for b = 10 to 1000 do
      for c = 10 to 1000 do
        if pythTriplets a b c && (a + b + c) = 1000 then
          ((* Prinf.printf "a = %d b = %d c = %d" a b c; *) failwith "Done")
        else ()
      done
    done
  done


let euler10 () =
  calcPrimesImp 2000000 |>
  Queue.fold ~init:0 ~f:(+)


let euler11 () =
  let ar2d = [|

[| 08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08 |];
[| 49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00 |];
[| 81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65 |];
[| 52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91 |];
[| 22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80 |];
[| 24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50 |];
[| 32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70 |];
[| 67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21 |];
[| 24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72 |];
[| 21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95 |];
[| 78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92 |];
[| 16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57 |];
[| 86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58 |];
[| 19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40 |];
[| 04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66 |];
[| 88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69 |];
[| 04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36 |];
[| 20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16 |];
[| 20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54 |];
[| 01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 48 |]

    |]

  in

  let prod = ref 0 in

  for i = 0 to 19 do
    for j = 0 to 19 do

      
      if i < 17 && j < 17 then
        let diag = ar2d.(i).(j) * ar2d.(i + 1).(j + 1) * ar2d.(i + 2).(j + 2) * ar2d.(i + 3).(j + 3) in
        printf "diag = %d %d X %d X %d X %d\n" diag ar2d.(i).(j)  ar2d.(i + 1).(j + 1) ar2d.(i + 2).(j + 2)  ar2d.(i + 3).(j + 3);
        if diag > !prod then prod := diag else ();
      else ();

      if i < 17 && j > 2 then
        let diag2 = ar2d.(i).(j) * ar2d.(i + 1).(j - 1) * ar2d.(i + 2).(j - 2) * ar2d.(i + 3).(j - 3) in
        printf "diag2 = %d %d X %d X %d X %d i = %d j = %d\n" diag2 ar2d.(i).(j)  ar2d.(i + 1).(j - 1) ar2d.(i + 2).(j - 2)  ar2d.(i + 3).(j - 3) i j ;
        if diag2 > !prod then prod := diag2 else ();
      else ();

      if i < 17 then
        let down = ar2d.(i).(j) * ar2d.(i + 1).(j) * ar2d.(i + 2).(j) * ar2d.(i + 3).(j) in
        printf "down = %d %d X %d X %d X %d\n" down ar2d.(i).(j)  ar2d.(i + 1).(j) ar2d.(i + 2).(j)  ar2d.(i + 3).(j);
        if down > !prod then prod := down else ();
      else ();
      

      if j < 17 then
        let right = ar2d.(i).(j) * ar2d.(i).(j + 1) * ar2d.(i).(j + 2) * ar2d.(i).(j + 3) in
        printf "right = %d %d X %d X %d X %d\n" right ar2d.(i).(j)  ar2d.(i).(j+1) ar2d.(i).(j+2)  ar2d.(i).(j+3);      
        if right > !prod then prod := right else ();
      else ();
    done;
  done; !prod


(* let euler12 () = *)
(*   (\* let primes = calcPrimesImp 1000000 in *\) *)
(*   (\* let primeSet = Hash_set.of_list (module Int) (Queue.to_list primes) in *\) *)
  
(*   (\* printf "primes generated\n"; *\) *)
(*   let q =  generateTriangNumbers 11000 in *)
(*   printf "size %d\n" (Queue.length q); *)

(*   (\* Queue.filter q ~f:(fun x -> not (Hash_set.mem primeSet x)) |>   *\) Queue.map q ~f:(fun x -> (x, (countDivisorsImp x))) |> Queue.filter ~f:(fun (_,y) -> y >= 400) *)
(*          |> Queue.iter ~f:(fun (x,y) -> printf "num: %d divisors %d\n" x y) *)
(*   (\* let len = Queue.length q in *\) *)
  (* for i = 0 to len - 1 do *)
  (*   let el = (Queue.get q i) in *)
  (*   printf "i = %d el = %d\n" i el; *)
  (*   let divCount = countDivisorsImp el  in  *)
  (*   printf "count %d\n" divCount; *)
   (*  if divCount % 100 = 0 then *)
   (*    printf "index %d Element %d count %d\n" i el divCount *)
   (*  else (); *)
    
   (*  if divCount = 500 then *)
   (*    (failwith "Found") *)
   (* else (); *)
  (* done *)

let euler12' () =
  let q = generateTriangNumbers 14000 in
  let table = Hashtbl.create (module Int) ~size:(Queue.length q) ~growth_allowed:true in
  let table = computeDivisors table 10000 in
  let len = Queue.length q in
  for i = 13000 to len - 1 do
    let el = (Queue.get q i) in
    (* printf "i = %d el = %d\n" i el; *)
    let tbl = countDivisorsImp table el in
    printf "count %d\n" (Hashtbl.length table); 
    let set = Hashtbl.find_exn tbl el in
    let len = Hash_set.length set in 
    if len >= 5000 then
      (printf "index %d Element %d count %d\n" i el len;
    Stdlib.flush_all())
    else ();   
  done; table
  
(*  |> Queue.map ~f:(fun x -> countDivisors x) |> Queue.max_elt ~compare:(Int.compare) in *)
  (* match q with *)
  (* | Some x -> printf "max divisors %d \n" x *)
  (* | None -> printf "garbage\n" *)


  (* let primes = calcPrimesImp 1000000 in *)
  (* let primeSet = Hash_set.of_list (module Int) (Queue.to_list primes) in *)
  
  (* printf "primes generated\n"; *)

(* let lwtTest () = *)
(*   let  open Lwt in *)
(*   Lwt_io.printf "bla\n" >>= fun _ -> () *)


let () =

  Caml.Gc.set { (Caml.Gc.get()) with Caml.Gc.verbose = 0x01e; Caml.Gc.minor_heap_size = 262144 * 4; Caml.Gc.allocation_policy = 2 };
  let cx = Caml.Gc.get() in
  
  printf "minor heap size %d stack limit %d custom %d\n" cx.minor_heap_size cx.stack_limit  cx.custom_minor_max_size;
  let t = euler12'() in
  let lst = Hashtbl.to_alist t in
  List.filter lst ~f:(fun (_,y) -> (Hash_set.length y >= 500)) |>
  List.iter  ~f:(fun (x,y) -> printf "el %d divisor count %d\n" x (Hash_set.length y))
  (* Lwt_main.run (lwtTest ()) *)
  (* euler12' () *)
  (* let q = generateTriangNumbers 10 in *)
  (* Queue.iter q ~f:(fun x -> printf "%d " x) *)
  (* let sum = euler11 () in *)
  (* printf "%d\n" sum *)
  (* euler9 () *)
  (* let prod, subStr = euler8 () in *)
  (* printf "%d %s\n" prod subStr *)
  (* let x = euler6List () in *)
  (* printf "%d\n" x *)
  (* let ar = findLargestPalindrome () in *)
  (* Array.sort ar ~compare:(Int.compare); *)
  (* printf "palindrom: %d\n" (Array.last ar) *)
  


