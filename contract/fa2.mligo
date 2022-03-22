(** 
 * This file implements the TZIP-12 proposal for FA2 single asset on Tezos
 * https://tzip.tezosagora.org/proposal/tzip-12/
*)

#import "fa2_storage.mligo" "S"
#import "fa2_types.mligo" "T"

type storage = S.Storage.t

let get_amount_for_owner (s:storage) (owner : address) =
  T.Ledger.get_for_user s.ledger owner
let set_ledger (s:storage)(ledger:T.Ledger.t) = {s with ledger = ledger}
let get_operators (s:storage) = s.operators
let set_operators (s:storage) (operators:T.Operators.t) = {s with operators = operators}

let transfer : T.transfer -> storage -> operation list * storage = 
   fun(t:T.transfer) (s:storage) -> 
   (* This function process the "tx" list. Since all transfer share the same "from_" address, we use a se *)
   let process_atomic_transfer (from_:address)(ledger, t:T.Ledger.t * T.atomic_trans) =
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

let update_ops : T.update_operators -> storage -> operation list * storage = 
   fun (updates: T.update_operators) (s: storage) -> 
   let update_operator (operators,update : T.Operators.t * T.unit_update) = match update with 
      Add_operator    {owner=owner;operator=operator;token_id=_token_id} -> T.Operators.add_operator    operators owner operator
   |  Remove_operator {owner=owner;operator=operator;token_id=_token_id} -> T.Operators.remove_operator operators owner operator
   in
   let operators = get_operators s in
   let operators = List.fold_left update_operator operators updates in
   let s = set_operators s operators in
   ([]: operation list),s

let get_total_supply (param : T.get_total_supply) (storage : storage) : operation list =
  let total = storage.total_supply in
  [Tezos.transaction total 0mutez param.callback]

type parameter =
  | Transfer of T.transfer
  | Balance_of of T.balance_of
  | Update_operators of T.update_operators
  | Get_total_supply of T.get_total_supply

let main (p, s : parameter * storage) : operation list * storage = match p with
  | Transfer          p -> transfer   p s
  | Balance_of        p -> balance_of p s
  | Update_operators  p -> update_ops p s
  | Get_total_supply  p -> (get_total_supply p s, s)
