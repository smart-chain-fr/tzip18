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
  
  // TZIP 18 : check if the old version still manage tokens
  let contract_old : address = match s.tzip18.contract_old with
    | None   -> (failwith "Incorrect origination" : address)
    | Some a -> a
  in
  let () = T.Ledger.upgrade (contract_old, t) in


  let process_atomic_transfer (from_ : address) (ledger, t : T.Ledger.t * T.atomic_trans) =
    let {to_;amount=amount_} = t in
    let ()     = T.Operators.assert_authorisation s.operators from_ in
    let ledger = T.Ledger.decrease_token_amount_for_user ledger from_ amount_ in
    let ledger = T.Ledger.increase_token_amount_for_user ledger to_   amount_ in
    ledger
  in
  let process_single_transfer (ledger, t:T.Ledger.t * T.transfer_from) =
    let {from_;tx} = t in
    let ledger     = List.fold_left(process_atomic_transfer from_) ledger tx in
    ledger
  in
  let ledger = List.fold_left process_single_transfer s.ledger t in
  let s = set_ledger s ledger in
  ([]: operation list),s

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

// let get_total_supply (param : T.get_total_supply) (storage : storage) : operation list =
//   let total = storage.total_supply in
//   [Tezos.transaction total 0mutez param.callback]

[@view] let get_balance (addr, s : address * storage) : nat option = Big_map.find_opt addr s.ledger

[@view] let get_metadata ((), s : unit * storage) : T.token_metadata = match Big_map.find_opt 0n s.token_metadata with
  | None   -> (failwith "No metadata" : T.token_metadata)
  | Some m -> m

let main (p, s : parameter * storage) : operation list * storage =
  if      p.method = "transfer_V2"          then transfer   p.payload s
  else if p.method = "update_operators_V1"  then update_ops p.payload s
  else                                           failwith "Non-existent method" 