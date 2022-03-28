(**
 * This file implements the TZIP-18 proposal on Tezos
 * https://tzip.tezosagora.org/proposal/tzip-18/
 * 
 * The design supports :
 * - adding/removing entrypoints
 * - changing an entrypoint's code and parameter type
 * - changing the storage's content and type
 * - contract address immutability
*)

(* =============================================================================
 * Types Definition
 * ============================================================================= *)

type call_contract = { 
  entrypoint_name : string; 
  payload         : bytes;
}

type ep = {
  addr       : address;
  parameters : string;
  is_view    : bool;
}

type ep_operation = {
  name        : string;
  is_removed  : bool;
  entrypoint  : ep option;
}

type token_metadata = {
  token_id   : nat;
  token_info : (string,bytes) map;
}

(* =============================================================================
 * Storage
 * ============================================================================= *)
type storage = {
  governance_proxy : address ;
  entrypoints : (string, ep) big_map; 
  token_metadata : (nat, token_metadata) big_map;
}

(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

// the proxy function 
let call_contract (param : call_contract) (storage : storage) : operation list * storage = 
  let op_call_contract : operation = 
    let amt = Tezos.amount in 
    let entry : ep = 
      match Big_map.find_opt param.entrypoint_name storage.entrypoints with 
      | None   -> (failwith "NO_ENTRYPOINT_FOUND" : ep)
      | Some a -> a in
    let destination_address = entry.addr in
    let destination_contract = 
      match (Tezos.get_contract_opt destination_address : bytes contract option) with 
      | None   -> (failwith "NO_CONTRACT_FOUND_AT_THIS ADDRESS" : bytes contract)
      | Some c -> c in 
    let () = failwith "TEST 0" in
    Tezos.transaction param.payload amt destination_contract
  in
  ([op_call_contract] : operation list), storage

// the governance proxy contract can update entrypoints 
let upgrade (la : (ep_operation list * address)) (s : storage) : operation list * storage = 
  let () = assert_with_error (Tezos.sender = s.governance_proxy) "PERMISSIONS_DENIED" in
  let rec update_storage ((l, m) : ep_operation list * (string, ep) big_map) : (string, ep) big_map =
    match l with
    | []      -> m
    | x :: xs ->
      let b : (string, ep) big_map = match x.entrypoint with
        | None     -> if x.is_removed then Big_map.remove x.name m else m
        | Some _ep -> 
          if not x.is_removed then match x.entrypoint with
          | None   -> m
          | Some c -> Big_map.update x.name (Some c) m
          else m
      in
      update_storage (xs, b)
  in
  let (upgraded_ep_list, new_metadata_address) : ep_operation list * address = la in
  let new_entrypoints : (string, ep) big_map = update_storage (upgraded_ep_list, s.entrypoints) in
  (([] : operation list), {s with entrypoints = new_entrypoints})
  // let new_token_metadata :(nat, token_metadata) big_map = Tezos.call_view "get_metadata" new_metadata_address in
  // (([] : operation list), {s with entrypoints = new_entrypoints; token_metadata = new_token_metadata})

(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// contract users can get the address through an on-chain view if they would rather 
// [@view] let get_entrypoint_address (entrypoint, storage : string * storage) : address = 
//     match Big_map.find_opt entrypoint storage.entrypoints with 
//     | None -> (failwith error_NO_ENTRYPOINT_FOUND : address)
//     | Some a -> a 

type parameter = 
  | CallContract of call_contract
  | Upgrade      of ep_operation list * address

let main (p, s : parameter * storage) : operation list * storage = match p with
  | CallContract p -> call_contract p s
  | Upgrade      p -> upgrade       p s
