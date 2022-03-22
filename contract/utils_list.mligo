(**
 * Empty constructor 
 *)
[@inline]
let construct (type kind) (size : nat) (wanted_type : kind) : kind option list =
  let rec construct (type kind) (size : nat) (wanted_type : kind) (res : kind option list) : kind option list =
    if (size = 0n) then res
    else construct (abs(size-1n)) wanted_type ((None: kind option) :: res) in
  construct size  wanted_type ([] : kind option list)

(**
 * Last 
 *)
[@inline]
let last (type kind) (lst1 : kind list) : kind =
  let rec last (type kind) (lst1 : kind list) : kind =
    match lst1 with 
    | []         -> failwith "The list is empty"
    | hd1 :: tl1 -> (
      match tl1 with
      | []         -> hd1
      | hd2 :: tl2 -> last (tl1) ) 
  in
  last (lst1)

(**
 * Reversing
 *)
[@inline]
let reverse (type kind) (lst1 : kind list) : kind list =
  let rec reverse (type kind) ((lst1, res) : kind list * kind list) : kind list =
    match lst1 with
    | [] -> res
    | hd1 :: tl1 -> reverse (tl1, (hd1 :: res)) in
  reverse (lst1, ([] : kind list))

(**
 * Concatenation
 *)
[@inline]
let concat (type kind) (lst1 : kind list) (lst2 : kind list) : kind list =
  let rec concat (type kind) ((lst1r, lst2) : kind list * kind list) : kind list =
    match lst1r with
    | []         -> lst2
    | hd1 :: tl1 -> concat (tl1, hd1 :: lst2) in
  let lst1r : kind list = reverse lst1 in
  concat (lst1r, lst2)

(**
 * Get an element by his number position
 *)
[@inline]
let find (type kind) (position : nat) (lst1 : kind list) : kind =
  let rec get (type kind) ((position, lst1) : nat * kind list) : kind =
    match lst1 with
    | []         -> failwith "Position is highter than list length"
    | hd1 :: tl1 ->
      if (position = 0n) then hd1
      else get (abs(position - 1n), tl1) in
  get (position, lst1)

(**
 * Set an element by his number position
 *)
[@inline]
let set (type kind) (element : kind) (position : nat) (lst1 : kind list) : kind list =
  let rec set (type kind) ((element, position, lst1, res) : kind * nat * kind list * kind list) : kind list =
    match lst1 with
    | []         -> failwith "Position is highter than list length"
    | hd1 :: tl1 ->
      if (position = 0n) then 
        let lst2 : kind list = reverse (element :: res) in
        concat lst2 tl1
      else set (element, abs(position - 1n), tl1, hd1 :: res) in
  set (element, position, lst1, ([] : kind list))

(**
 * Insertion
 *)
[@inline]
let insert (type kind) (element : kind) (position : nat) (lst1 : kind list) : kind list =
  let rec insert (type kind) ((element, position, lst1, lst2) : kind * nat * kind list * kind list) : kind list =
    match lst1, position with 
    | []        , pos -> failwith "Position is highter than list length"
    | hd1 :: tl1, pos -> 
      if (pos = 0n) then
        let lst3 : kind list = element::lst2 in
        let lst4 : kind list = reverse lst3 in 
        concat lst4 lst1
      else
        insert (element, abs(pos - 1n), tl1, hd1 :: lst2) in
  insert (element, position, lst1, ([] : kind list))

(**
 * Drop
 *)
[@inline]
let drop (type kind) (position : nat) (lst1 : kind list) : kind list =
  let rec drop (type kind) ((position, lst1, lst2) : nat * kind list * kind list) : kind list =
    match lst1 with 
    | []         -> failwith "Position is highter than list length"
    | hd1 :: tl1 -> 
      if (position = 0n) then
        let lst3 : kind list = reverse lst2 in 
        concat lst3 tl1
      else
        drop (abs(position - 1n), tl1, hd1 :: lst2) in
  drop (position, lst1, ([] : kind list))

(**
 * take
 *)
[@inline]
let take (type kind) (i : nat) (lst : kind list) : kind list =
  let rec take (type kind) ((i, lst, res) : nat * kind list * kind list) : kind list =
    if (i = 0n ) then reverse res
    else match lst with
      | []         -> reverse res
      | hd1 :: tl1 -> take (abs(i-1n), tl1, hd1 :: res) in
  take (i, lst, ([] : kind list))
  
(**
 * Slice
 *)
[@inline]
let slice (type kind) (i : nat) (k : nat) (lst : kind list) : kind list =
  let rec slice (type kind) ((i, k, lst) : nat * nat * kind list) : kind list =
    if (i = 0n ) then 
      let extract : nat = abs(k-i) in
      take extract lst
    else match lst with
      | []         -> []
      | hd1 :: tl1 -> slice (abs(i-1n), k, tl1) in
  slice (i, k, lst)
  
(**
 * Split
 *)
[@inline]
let split (type kind) (i : nat) (lst : kind list) : kind list * kind list =
  let rec split (type kind) ((i, lst1, lst2): nat * kind list * kind list) : kind list * kind list =
    if (i = 0n ) then let lstr = reverse lst2 in (lstr, lst1)
    else match lst1 with
      | []         -> let lstr = reverse lst2 in (lstr, lst1)
      | hd1 :: tl1 -> split (abs(i-1n), tl1, hd1 :: lst2) in
  split (i, lst, ([] : kind list))

(**
 * Rotate to the left
 *)
[@inline]
let rotate (type kind) (i : nat) (lst : kind list) : kind list =
  let rec rotate (type kind) ((i, lst, res) : nat * kind list * kind list) : kind list =
    if (i = 0n ) then 
      let lstr = reverse res in
      concat lst lstr
    else match lst with
      | []         -> reverse res
      | hd1 :: tl1 -> rotate (abs(i-1n), tl1, hd1 :: res) in
  rotate (i, lst, ([] : kind list))


// (**
// * equal (michelson version / not working for composed types)
// *)
// [@inline]
// let equalm (type a) (val_a : a) (val_b : a) : bool =
//   [%Michelson ({|{ UNPAIR; COMPARE; EQ }|} : a * a -> bool)] (val_a, val_b)


(**
* equal (bytes version)
* WARNING : Two lambda can be packed equal whereas they are different
*)
[@inline]
let equal (type a) (val_a: a) (val_b: a): bool =
    (Bytes.pack val_a) = (Bytes.pack val_b)

(**
 * Remove
 *)
[@inline]
let remove (type kind) (element : kind) (lst : kind list) : kind list =
  let rec remove (type kind) ((element, lst, res) : kind * kind list * kind list) : kind list =
    match lst with
      | []         -> reverse res
      | hd1 :: tl1 -> 
        let is_equal = equal hd1 element in
        if is_equal then remove (element, tl1, res)
        else remove (element, tl1, hd1 :: res) 
    in
  remove (element, lst, ([] : kind list))

