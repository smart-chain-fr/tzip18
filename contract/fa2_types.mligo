type tzip18 = {
  contract_old : address option;
  version_old : nat option;
  version_current : nat;
  contract_next : address option;
  version_next : nat option;
  is_in_use : bool;
}

module Ledger = struct
  type owner  = address
  type amount_ = nat
  type t = (owner, amount_) big_map
  
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
    let sender_ = Tezos.sender in
    if (sender_ = from_) then ()
    else 
    let authorized = match Big_map.find_opt from_ operators with
        Some (a) -> a | None -> Set.empty
    in if Set.mem sender_ authorized then ()
    else failwith "FA2_NOT_OPERATOR"

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

type token_metadata = {
  token_id   : nat;
  token_info : (string,bytes) map;
}

type operator = {
  owner    : address;
  operator : address;
  token_id : nat; 
}

type unit_update      = Add_operator of operator | Remove_operator of operator
type update_operators = unit_update list

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