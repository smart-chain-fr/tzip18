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

type call_type = {
  method  : string;
  payload : bytes;
}

type call_contract = { 
  entrypoint_name : string; 
  payload         : bytes;
}

type ep = {
  method  : string;
  addr    : address;
  is_view : bool;
}

type ep_operation = {
  name        : string;
  is_removed  : bool;
  entrypoint  : ep option;
}

type change_version = {
  old : address;
  new : address;
}

type token_metadata = {
  token_id   : nat;
  token_info : (string, bytes) map;
}

(* =============================================================================
 * Storage -- Todo : had global Ledger to reflect current version for wallet ?
 * ============================================================================= *)
type storage = {
  governance : address ;
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
    let entry : ep = match Big_map.find_opt param.entrypoint_name storage.entrypoints with
      | None   -> (failwith "No entrypoint found" : ep)
      | Some a -> a in

    match (Tezos.get_contract_opt entry.addr : call_type contract option) with
    | None          -> (failwith "No contract found at this address" : operation)
    | Some contract -> 
      let call_param : call_type =  {
        method  = entry.method;
        payload = param.payload;
      } in
      Tezos.transaction call_param amt contract
  in
  ([op_call_contract] : operation list), storage

// the governance proxy contract can update entrypoints 
let upgrade (param : (ep_operation list * change_version option  * address)) (s : storage) : operation list * storage = 
  let () = assert_with_error (Tezos.sender = s.governance) "Permission denied" in
  let (upgraded_ep_list, change_version_opt, new_metadata_address) : ep_operation list * change_version option * address = param in
  let rec update_storage ((l, m) : ep_operation list * (string, ep) big_map) : (string, ep) big_map =
    match l with
    | []      -> m
    | x :: xs ->
      let b : (string, ep) big_map = match x.entrypoint with
        | None     -> if x.is_removed then Big_map.remove x.name m else m
        | Some _ep -> 
          if not x.is_removed then match x.entrypoint with
          | None   -> m
          | Some c ->
            Big_map.update x.name (Some c) m
          else m
      in
      update_storage (xs, b)
  in
  let new_entrypoints : (string, ep) big_map = update_storage (upgraded_ep_list, s.entrypoints) in

  let new_token_metadata_0n_opt : token_metadata option =
    Tezos.call_view "get_metadata" unit new_metadata_address in
  let new_token_metadata_0n : token_metadata = match new_token_metadata_0n_opt with
    | Some value -> value
    | None       -> failwith "Non-existent meta"
  in
  let new_token_metadata : (nat, token_metadata) big_map =
    Big_map.update 
      (0n : nat)
      (Some(new_token_metadata_0n))
      s.token_metadata
  in

  //Ask the old version put the new contract as master
  match change_version_opt with
  | None        -> (([] : operation list), {s with entrypoints = new_entrypoints ; token_metadata = new_token_metadata})
  | Some change ->
    let op_change : operation = 
      match (Tezos.get_contract_opt change.old : call_type contract option) with
    | None          -> (failwith "No contract found at this address" : operation)
    | Some contract -> 
        let amt = Tezos.amount in 
        let payload : address = change.new in
        let call_param : call_type = {
          method  = ("change_version" : string); 
          payload = Bytes.pack payload;
        } in
        Tezos.transaction call_param amt contract
    in
    (([op_change] : operation list), {s with entrypoints = new_entrypoints ; token_metadata = new_token_metadata})

type parameter = 
  | Call    of call_contract
  | Upgrade of ep_operation list * change_version option * address

let main (p, s : parameter * storage) : operation list * storage = match p with
  | Call    p -> call_contract p s
  | Upgrade p -> upgrade       p s
