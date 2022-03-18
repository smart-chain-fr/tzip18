// A proxy contract for storage that allows for storage upgrades 
#import "fa12_types.mligo" "T"

// TODO : 
//  - composable storage that allows you to migrate storage over time. 
//        Using contract views, this is how you transfer *big maps* : if searching for 
//        a value, and it's not there, then try to fetch it; the same goes all the way back 
//        to the v0 storage. You make a chain of storage contracts. It's really cheap to 
//        migrate the first time using views, then it's in the present database in the format it 
//        should be.
//  - you'll need to be able to delete the data from the old smart contract when it's ported over, 
//    otherwise people might be able to double count. so an entrypoint and something in storage that 
//    indicates the "next vesion" of storage on-chain.
//  - you just need views exposing all your big maps (is this a security issue? surely not?)

(* =============================================================================
 * Storage
 * ============================================================================= *)

module Storage = struct
  type token_id = nat
  type t = {
    ledger : T.ledger;
    allowances : T.allowances;
    admin : address;
    total_supply : nat;
    token_metadata : (nat, T.token_metadata_entry) big_map;
  }
end

type storage = Storage.t

type result = operation list * storage


(* =============================================================================
 * Entrypoint Type Definition
 * ============================================================================= *)


(* =============================================================================
 * Error Codes
 * ============================================================================= *)


(* =============================================================================
 * Aux Functions
 * ============================================================================= *)


(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)




(* =============================================================================
 * Contract Views
 * ============================================================================= *)

// expose all your big maps to a view
// [@view] view1 (input : input) : output = ...

(* =============================================================================
 * Main Function
 * ============================================================================= *)


