// A proxy contract for smart contract upgradeability 
// This contract 


(* =============================================================================
 * Storage
 * ============================================================================= *)


type storage = {
    governance_proxy : address ;
    entrypoints : (string, address) big_map ; 
    entrypoint_metadata : (string, string) big_map ; 
}


type result = operation list * storage 


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type call_contract = { entrypoint : string ; payload : bytes ; }
type update_entrypoints = { 
    new_entrypoint_name : string ; 
    new_entrypoint_address : address option ; // none if removing an entrypoint 
    new_entrypoint_type : string ; 
}

type entrypoint = 
| CallContract of call_contract
| UpdateEntrypoints of update_entrypoints list 

(* =============================================================================
 * Error Codes
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n 
let error_NO_ENTRYPOINT_FOUND = 1n
let error_NO_CONTRACT_FOUND = 2n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)


(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

// the proxy function 
let call_contract (param : call_contract) (storage : storage) : result = 
    let op_call_contract = 
        let amt = Tezos.amount in 
        let entrypoint_address = 
            match Big_map.find_opt param.entrypoint storage.entrypoints with 
            | None -> (error_NO_ENTRYPOINT_FOUND : address)
            | Some a -> a in 
        let destination_contract = 
            match (Tezos.get_contract_opt entrypoint_address : bytes contract option) with 
            | None -> (error_NO_ENTRYPOINT_FOUND : bytes contract)
            | Some c -> c in 
        Tezos.transaction param.payload amt destination_contract in
    [ op_call_contract ; ], storage 

// the governance proxy contract can update entrypoints 
let update_entrypoints (param : update_entrypoints list) (storage : storage) : result = 
    // check permissions 
    if Tezos.sender <> storage.governance_proxy then (failwith error_PERMISSIONS_DENIED : result) else 
    ([] : operation list),
    { storage with 
        entrypoints = 
            List.fold 
            (fun (param, storage : update_entrypoints * storage) : (string, address) big_map -> 
                Big_map.update param.new_entrypoint_name param.new_entrypoint_address storage.entrypoints)
            param 
            storage ;
        entrypoint_metadata = 
            List.fold 
            (fun (param, storage : update_entrypoints * storage) : (string, address) big_map -> 
                Big_map.update param.new_entrypoint_name param.new_entrypoint_type storage.entrypoint_metadata)
            param 
            storage ; 
    }


(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// contract users can get the address through an on-chain view if they would rather 
[@view] let get_entrypoint_address (entrypoint, storage : string * storage) : address = 
    match Big_map.find_opt entrypoint storage.entrypoints with 
    | None -> (failwith error_NO_ENTRYPOINT_FOUND : address)
    | Some a -> a 

(* =============================================================================
 * Main Function
 * ============================================================================= *)

let main (param, storage : entrypoint * storage) : result = 
    match param with 
    | CallContract p -> 
        call_contract p storage 
    | UpdateEntrypoints p ->
        update_entrypoints p storage 


