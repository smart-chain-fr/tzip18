// A template to start coding a new contract
// TODO : only one proposal at a time

(* =============================================================================
 * Storage
 * ============================================================================= *)

type update_governance = {
    governing_party : address ; 
    governance_votes : nat ; 
}
type update_entrypoints = { 
    new_entrypoint_name : string ; 
    new_entrypoint_address : address option ; // none if removing an entrypoint 
    new_entrypoint_type : string ; 
}
type proposal_type = 
| Update_Governance of update_governance list 
| Update_Entrypoints of update_entrypoints
type proposal = { proposal_id : nat ; proposal_type : proposal_type ; }

type voter_data = [@layout:comb]{ voter : address ; proposal_id : nat ;}
type vote = 
| Yea of unit 
| Nay of unit 
| Abstain of unit 


type storage = {
    // governance
    governance : (address, nat) big_map ; 
    quorum_size : nat ; 
    // voting time period 
    proposal_duration : nat ;
    proposal_in_progress : bool ; 

    proposals : (proposal, timestamp) big_map ; 
    votes : (voter_data, vote) big_map ; 
}


type result = operation list * storage 


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)

type entrypoint = 
| Propose of proposal 
| Execute of unit


(* =============================================================================
 * Error Codes
 * ============================================================================= *)

let error_PERMISSIONS_DENIED = 0n

(* =============================================================================
 * Aux Functions
 * ============================================================================= *)


(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

let update_governance (param : update_governance) (storage : storage) : result = 

let update_


(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// [@view] view1 (input : input) : output = ...

(* =============================================================================
 * Main Function
 * ============================================================================= *)

let main (param, storage : entrypoint * storage) : result = 
    match param with 
    | // entrypoints 


