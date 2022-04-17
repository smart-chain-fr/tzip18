(** 
 * This file implements the TZIP-12 proposal for FA2 single asset on Tezos
 * https://tzip.tezosagora.org/proposal/tzip-12/
*)

#import "fa2_storage.mligo" "S"
#import "fa2_types.mligo" "T"

type storage = S.Storage.t

type parameter = {
  method  : string;
  payload : bytes;
}

let get_amount_for_owner (s:storage) (owner : address) =
  T.Ledger.get_for_user s.ledger owner
let set_ledger (s:storage)(ledger:T.Ledger.t) = {s with ledger = ledger}
let get_operators (s:storage) = s.operators
let set_operators (s:storage) (operators:T.Operators.t) = {s with operators = operators}

let transfer : bytes -> storage -> operation list * storage =
  fun(packed_param:bytes) (s:storage) ->
  let t_opt :  T.transfer option = (Bytes.unpack packed_param :  T.transfer option) in 
  let t :  T.transfer = match t_opt with 
    | None   -> (failwith "Incorrect bytes for transfer_V2 call" : T.transfer)
    | Some a -> a
  in
  
  // TZIP 18 : check if the old version still manage tokens for any address
  let contract_prev : address = match s.tzip18.contract_prev with
    | None   -> (failwith "Incorrect origination" : address)
    | Some a -> a
  in
  let (new_ledger, addresses_to_purge) : T.Ledger.t * address list  = T.Ledger.upgrade_ledger (contract_prev, t, s.ledger) in
      

  // Make classic treatment
  let process_atomic_transfer (from_ : address) (ledger, t : T.Ledger.t * T.atomic_trans) =
    let {to_;amount=amount_} = t in
    let ()     = T.Operators.assert_authorisation s.operators from_ in
    let ledger = T.Ledger.decrease_token_amount_for_user ledger from_ amount_ in
    let ledger = T.Ledger.increase_token_amount_for_user ledger to_   amount_ in
    ledger
  in
  let process_single_transfer (ledger, t : T.Ledger.t * T.transfer_from) =
    let {from_;tx} = t in
    let ledger     = List.fold_left(process_atomic_transfer from_) ledger tx in
    ledger
  in
  let new_ledger = List.fold_left process_single_transfer new_ledger t in

  let new_storage = set_ledger s new_ledger in

  // Put fa12 map to zero
  match s.tzip18.contract_prev with
  | None      -> ([]: operation list),new_storage
  | Some addr ->
    let op_purge : operation = 
      match (Tezos.get_contract_opt addr : T.call_type contract option) with
    | None          -> (failwith "No contract found at this address" : operation)
    | Some contract -> 
        let amt = Tezos.amount in 
        let payload : address list = addresses_to_purge in
        let call_param : T.call_type = {
          method  = ("purge_addresses" : string); 
          payload = Bytes.pack payload;
        } in
        Tezos.transaction call_param amt contract
    in
    ([op_purge] : operation list), new_storage

let balance_of : T.balance_of -> storage -> operation list * storage = 
   fun(b: T.balance_of) (s: storage) -> 
   let {requests;callback} = b in
   let get_balance_info (request : T.request) : T.callback =
      let {owner;token_id=_token_id} = request in
      let balance_ = get_amount_for_owner s owner    in
      {request=request;balance=balance_}
   in
   let callback_param = List.map get_balance_info requests in
   let operation = Tezos.transaction callback_param 0tez callback in
   ([operation]: operation list),s

let update_ops : bytes -> storage -> operation list * storage = 
  fun (packed_param : bytes) (s: storage) -> 
  let updates_opt : T.update_operators option = (Bytes.unpack packed_param : T.update_operators option) in 
  let updates : T.update_operators = match updates_opt with 
    | None   -> (failwith "Incorrect bytes for update_operators_V1 call" : T.update_operators)
    | Some a -> a
  in
   let update_operator (operators,update : T.Operators.t * T.unit_update) = match update with 
      Add_operator    {owner=owner;operator=operator;token_id=_token_id} -> T.Operators.add_operator    operators owner operator
   |  Remove_operator {owner=owner;operator=operator;token_id=_token_id} -> T.Operators.remove_operator operators owner operator
   in
   let operators = get_operators s in
   let operators = List.fold_left update_operator operators updates in
   let s = set_operators s operators in
   ([]: operation list),s

let change_version (packed_param : bytes) (storage : storage) : operation list * storage =
  let param_opt : address option = (Bytes.unpack packed_param : address option) in
  let param : address = match param_opt with 
    | None   -> (failwith "Incorrect bytes for transfer_V1 call" : address)
    | Some a -> a
  in
  (([] : operation list), {storage with tzip18.contract_next = Some(param) } )


let purge (packed_param : bytes) (store : storage) : operation list * storage =
  let param_opt : address list option = (Bytes.unpack packed_param : address list option) in
  let param : address list = match param_opt with 
    | None   -> (failwith "Incorrect bytes for transfer_V1 call" : address list)
    | Some a -> a
  in
  let ledger = store.ledger in

  // Update the ledger to zero
  let rec put_at_zero (addr_list, ledg : address list * (address, nat) big_map) : (address, nat) big_map =
    match addr_list with
    | []      -> ledg
    | x :: xs -> 
      let new_ledger : (address, nat) big_map = Big_map.update (x : address) (Some(0n)) ledg in
      put_at_zero (xs, new_ledger)
  in
  let new_ledger : (address, nat) big_map = put_at_zero (param, ledger) in
  (([] : operation list), {store with ledger = new_ledger})

[@view] let get_balance (addr, s : address * storage) : nat = match Big_map.find_opt addr s.ledger with
  | None   -> 0n
  | Some m -> m

[@view] let get_metadata ((), s : unit * storage) : T.token_metadata = match Big_map.find_opt 0n s.token_metadata with
  | None   -> (failwith "No metadata" : T.token_metadata)
  | Some m -> m

let main (p, s : parameter * storage) : operation list * storage =
  let _ = assert_with_error (Tezos.sender = s.tzip18.proxy) "Only the proxy can call this contract" in
  if      p.method = "transfer_V2"          then transfer   p.payload s
  else if p.method = "update_operators_V1"  then update_ops p.payload s
  else if p.method = "change_version"       then change_version p.payload s
  else if p.method = "purge_addresses"      then purge          p.payload s
  else                                           failwith "Non-existent method" 