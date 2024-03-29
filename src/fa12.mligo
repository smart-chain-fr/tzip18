(**
 * This file implements the TZIP-7 proposal for FA12 single asset on Tezos
 * https://tzip.tezosagora.org/proposal/tzip-7/
*)

#import "fa12_storage.mligo" "S"
#import "fa12_types.mligo" "T"

type storage = S.Storage.t

type parameter = {
  method  : string;
  payload : bytes;
}

let maybe (n : nat) : nat option =
  if n = 0n
  then (None : nat option)
  else Some n

let transfer (packed_param : bytes) (storage : storage) : operation list * storage =
  let param_opt : T.transfer option = (Bytes.unpack packed_param : T.transfer option) in
  let param : T.transfer = match param_opt with 
    | None   -> (failwith "Incorrect bytes for transfer_V1 call" : T.transfer)
    | Some a -> a
  in

  let allowances = storage.allowances in
  let ledger = storage.ledger in

  // Check allowance amount
  let allowances =
    if Tezos.get_source() = param.address_from
    then allowances
    else
      let allowance_key = { owner = param.address_from ; spender = Tezos.get_sender() } in
      let authorized_value =
        match Big_map.find_opt allowance_key allowances with
        | Some value -> value
        | None -> 0n in
      let authorized_value =
        match is_nat (authorized_value - param.value) with
        | None -> (failwith "NotEnough allowance" : nat)
        | Some authorized_value -> authorized_value in
      Big_map.update allowance_key (maybe authorized_value) allowances in

  // Check balance amount
  let ledger =
    let from_balance =
      match Big_map.find_opt param.address_from ledger with
      | Some value -> value
      | None -> 0n in
    let from_balance =
      match is_nat (from_balance - param.value) with
      | None -> (failwith "Not enough balance" : nat)
      | Some from_balance -> from_balance in
    Big_map.update param.address_from (maybe from_balance) ledger in

  // Find receiver and send update the ledger
  let ledger =
    let to_balance =
      match Big_map.find_opt param.address_to ledger with
      | Some value -> value
      | None -> 0n in
    let final_value : nat = to_balance + param.value in
    let to_balance : nat option = Some(final_value) in
    Big_map.update param.address_to to_balance ledger
  in
  (([] : operation list), { storage with ledger = ledger; allowances = allowances })    

let approve (packed_param : bytes) (storage : storage) : operation list * storage =
  let param_opt : T.approve option = (Bytes.unpack packed_param : T.approve option) in
  let param : T.approve = match param_opt with 
    | None   -> (failwith "Incorrect bytes for approve_V1 call" : T.approve)
    | Some a -> a
  in
  let allowances = storage.allowances in
  let allowance_key = { owner = Tezos.get_sender() ; spender = param.spender } in
  let previous_value =
    match Big_map.find_opt allowance_key allowances with
    | Some value -> value
    | None -> 0n in
  begin
    if previous_value > 0n && param.value > 0n
    then (failwith "Unsafe allowance change")
    else ();
    let allowances = Big_map.update allowance_key (maybe param.value) allowances in
    (([] : operation list), { storage with allowances = allowances })
  end


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

[@view] let get_balance (addr, s : address * storage) : nat =
  match Big_map.find_opt addr s.ledger with
  | None   -> 0n
  | Some m -> m

[@view] let get_metadata ((), s : unit * storage) : T.token_metadata =
  match Big_map.find_opt 0n s.token_metadata with
  | None   -> (failwith "No metadata" : T.token_metadata)
  | Some m -> m

let main (p, s : parameter * storage) : operation list * storage =
  let is_next_version_call : bool = match s.tzip18.contract_next with 
    | None            -> False
    | Some (contract) ->
      if ( Tezos.get_sender() =  contract ) then True 
      else False
  in
  let _ = assert_with_error (Tezos.get_sender() = s.tzip18.proxy || is_next_version_call)
    "Only the proxy or master can call this contract" in
  if      p.method = "transfer_V1"     then transfer       p.payload s
  else if p.method = "approve_V1"      then approve        p.payload s
  else if p.method = "change_version"  then change_version p.payload s
  else if p.method = "purge_addresses" then purge          p.payload s
  else                                  failwith "Non-existant method" 
