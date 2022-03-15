type transfer =
  [@layout:comb]
  { [@annot:from] address_from : address;
    [@annot:to] address_to : address;
    value : nat }

type approve =
  [@layout:comb]
  { spender : address;
    value : nat }

type allowance_key =
  [@layout:comb]
  { owner : address;
    spender : address }

type getAllowance =
  [@layout:comb]
  { request : allowance_key;
    callback : nat contract }

type getBalance =
  [@layout:comb]
  { owner : address;
    callback : nat contract }

type getTotalSupply =
  [@layout:comb]
  { request : unit ;
    callback : nat contract }

type ledger = (address, nat) big_map
type allowances = (allowance_key, nat) big_map

type token_metadata_entry = {
  token_id: nat;
  token_info: (string, bytes) map;
}
type storage =
  [@layout:comb]
  { ledger : ledger;
    allowances : allowances;
    admin : address;
    total_supply : nat;
    token_metadata : (nat, token_metadata_entry) big_map
  }

type parameter =
  | Transfer of transfer
  | Approve of approve
  | GetAllowance of getAllowance
  | GetBalance of getBalance
  | GetTotalSupply of getTotalSupply

type result = operation list * storage

[@inline]
let maybe (n : nat) : nat option =
  if n = 0n
  then (None : nat option)
  else Some n

let transfer (param : transfer) (storage : storage) : result =

  let allowances = storage.allowances in
  let ledger = storage.ledger in

  // Check allowance amount
  let allowances =
    if Tezos.sender = param.address_from
    then allowances
    else
      let allowance_key = { owner = param.address_from ; spender = Tezos.sender } in
      let authorized_value =
        match Big_map.find_opt allowance_key allowances with
        | Some value -> value
        | None -> 0n in
      let authorized_value =
        match is_nat (authorized_value - param.value) with
        | None -> (failwith "NotEnoughAllowance" : nat)
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
      | None -> (failwith "NotEnoughBalance" : nat)
      | Some from_balance -> from_balance in
    Big_map.update param.address_from (maybe from_balance) ledger in

  // 
  let ledger =
    let to_balance =
      match Big_map.find_opt param.address_to ledger with
      | Some value -> value
      | None -> 0n in
    let final_value : nat = to_balance + param.value in
    let to_balance : nat option = Some(final_value) in
    Big_map.update param.address_to to_balance ledger in
    (([] : operation list), { storage with ledger = ledger; allowances = allowances })    

let approve (param : approve) (storage : storage) : result =
  let allowances = storage.allowances in
  let allowance_key = { owner = Tezos.sender ; spender = param.spender } in
  let previous_value =
    match Big_map.find_opt allowance_key allowances with
    | Some value -> value
    | None -> 0n in
  begin
    if previous_value > 0n && param.value > 0n
    then (failwith "UnsafeAllowanceChange")
    else ();
    let allowances = Big_map.update allowance_key (maybe param.value) allowances in
    (([] : operation list), { storage with allowances = allowances })
  end

let getAllowance (param : getAllowance) (storage : storage) : operation list =
  let value =
    match Big_map.find_opt param.request storage.allowances with
    | Some value -> value
    | None -> 0n in
  [Tezos.transaction value 0mutez param.callback]

let getBalance (param : getBalance) (storage : storage) : operation list =
  let value =
    match Big_map.find_opt param.owner storage.ledger with
    | Some value -> value
    | None -> 0n in
  [Tezos.transaction value 0mutez param.callback]

let getTotalSupply (param : getTotalSupply) (storage : storage) : operation list =
  let total = storage.total_supply in
  [Tezos.transaction total 0mutez param.callback]

let main (param, storage : parameter * storage) : result =
  begin
    match param with
    | Transfer param -> transfer param storage
    | Approve param -> approve param storage
    | GetAllowance param -> (getAllowance param storage, storage)
    | GetBalance param -> (getBalance param storage, storage)
    | GetTotalSupply param -> (getTotalSupply param storage, storage)
  end