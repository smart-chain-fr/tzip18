// Tzip 18 types
type tzip18 = {
  proxy : address;
  version : nat;
  contract_prev : address option;
  contract_next : address option;
}

type call_type = {
  method  : string;
  payload : bytes;
}


type atomic_trans = {
  to_      : address;
  amount   : nat;
}

type transfer_from = {
  from_ : address;
  tx    : atomic_trans list
}

type transfer = transfer_from list

type request = {
  owner    : address;
  token_id : nat;
}

type callback = {
  request : request;
  balance : nat;
}

type balance_of = {
  requests : request list;
  callback : callback list contract;
}

type get_total_supply = { 
  request : unit ; 
  callback : nat contract
  }

type operator = {
  owner    : address;
  operator : address;
  token_id : nat; 
}

type unit_update      = Add_operator of operator | Remove_operator of operator
type update_operators = unit_update list

type token_metadata = {
  token_id   : nat;
  token_info : (string,bytes) map;
}


module Ledger = struct
  type owner  = address
  type amount_ = nat
  type t = (owner, amount_) big_map
  

  //Tzip 18 method for lazy update
  let upgrade_ledger (contract_old, trans, ledger : address * transfer * t) : t * address list =

    let address_list_to_update : address list = // list of the address to update
      let rec decompose_list (trans, addr_list : transfer * address list) : address list =
        match trans with 
        | []      -> addr_list
        | x :: xs ->
          let added_addr : address = x.from_ in
          let new_addr_list : address list = added_addr :: addr_list in
          let atomic_list : atomic_trans list = x.tx in
          let rec get_tx_address (atomic_list, new_addr_list : atomic_trans list * address list) : address list =
            match atomic_list with 
            | []      -> new_addr_list
            | x :: xs -> 
              let new_addr_list : address list = x.to_ :: new_addr_list in
              get_tx_address (xs, new_addr_list)
          in
          let new_addr_list :  address list = get_tx_address (atomic_list, new_addr_list) in
          decompose_list (xs, new_addr_list)
      in
      decompose_list (trans, ([] : address list))
    in


    //Determine the addresses to be upgraded
    let view_balance (addr : address) : nat =
      let old_balance_opt : nat option = Tezos.call_view "get_balance" addr contract_old in
      let old_balance : nat = match old_balance_opt with
        | Some value -> value
        | None       -> failwith "Non-existent meta"
      in
      old_balance
    in


    let addresses_to_put_at_zero : address list * nat list =
      let rec view_and_upgrade_map (to_check, to_put_at_zero, tokens : address list * address list * nat list) : address list * nat list  =
        match to_check with
        | []      -> to_put_at_zero, tokens
        | x :: xs -> 
          let bal = view_balance x in
          if ( bal > 0n ) then view_and_upgrade_map(xs, x :: to_put_at_zero, bal :: tokens)
          else view_and_upgrade_map(xs, to_put_at_zero, tokens)
      in
      view_and_upgrade_map (address_list_to_update, ([] : address list), ([] : nat list))
    in

    let (addr_list, values_list) : address list * nat list  = addresses_to_put_at_zero in

    let upgrade_to_v2 (addr_list, values_list, ledger : address list * nat list * t) : t =
      let rec upgrade_to_v2 (addr_list, values_list, ledger : address list * nat list * t) : t =
        match addr_list, values_list with
        [], [] -> ledger
        | [], _lst -> failwith "size don't match"
        | _lst, [] -> failwith "size don't match"
        | x::xs, y::ys ->
          let new_decimal_rate : nat = 1000n in
          let new_balance = y * new_decimal_rate in
          let new_ledger = Map.update (x : address) (Some new_balance) ledger in
          upgrade_to_v2 (xs, ys, new_ledger)
      in
      upgrade_to_v2 (addr_list, values_list, ledger )
    in

    let new_ledger : t = match List.head_opt addr_list with
      | Some a -> upgrade_to_v2(addr_list, values_list, ledger)
      | None   -> ledger
    in

    (new_ledger, address_list_to_update)




  let get_for_user    (ledger:t) (owner: owner) : amount_ =
  match Big_map.find_opt owner ledger with 
  | Some (tokens) -> tokens
  | None          -> 0n

  let update_for_user (ledger:t) (owner: owner) (amount_ : amount_) : t = 
    Big_map.update owner (Some amount_) ledger

  let decrease_token_amount_for_user (ledger : t) (from_ : owner) (amount_ : amount_) : t = 
    let tokens = get_for_user ledger from_ in
    
    let () = assert_with_error (tokens >= amount_) "FA2_INSUFFICIENT_BALANCE" in
    let tokens = abs(tokens - amount_) in
    let ledger = update_for_user ledger from_ tokens in
    ledger 

    let increase_token_amount_for_user (ledger : t) (to_   : owner) (amount_ : amount_) : t = 
    let tokens = get_for_user ledger to_ in
    let tokens = tokens + amount_ in
    let ledger = update_for_user ledger to_ tokens in
  ledger 
end

module Operators = struct
  type owner    = address
  type operator = address
  type token_id = nat
  type t = (owner, operator set) big_map

(** if transfer policy is Owner_or_operator_transfer *)
  let assert_authorisation (operators : t) (from_ : address) : unit = 
    let sender_ = Tezos.source in
    if (sender_ = from_) then ()
    else 
    let authorized = match Big_map.find_opt from_ operators with
        Some (a) -> a | None -> Set.empty
    in if Set.mem sender_ authorized then ()
    else failwith "FA2_NOT_SENDER_NOR_OPERATOR"

   let assert_update_permission (owner : owner) : unit =
      assert_with_error (owner = Tezos.sender) "The sender can only manage operators for his own token"

   let add_operator (operators : t) (owner : owner) (operator : operator) : t =
      if owner = operator then operators (* assert_authorisation always allow the owner so this case is not relevant *)
      else
         let () = assert_update_permission owner in
         let auths = match Big_map.find_opt owner operators with
            Some (os) -> os | None -> Set.empty in
         let auths  = Set.add operator auths in
         Big_map.update owner (Some auths) operators
         
   let remove_operator (operators : t) (owner : owner) (operator : operator) : t =
      if owner = operator then operators
      else
         let () = assert_update_permission owner in
         let auths = match Big_map.find_opt owner operators with
         None -> None | Some (os) ->
            let os = Set.remove operator os in
            if (Set.size os = 0n) then None else Some (os)
         in
         Big_map.update owner auths operators
end

