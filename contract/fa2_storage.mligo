// A proxy contract for storage that allows for storage upgrades 
#import "fa2_types.mligo" "T"

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
 * 
 * The module Storage must be initialized at origination with the old contract address
 * old contract address
 * ============================================================================= *)

module Storage = struct
  module Upgradable = struct
    type master_proxy = address
    type contract_old = address
    type version_old = nat
    type version_current = nat
    type contract_next = address
    type version_next = nat option
    type is_in_use = bool
  end
  type token_id = nat
  type t = {
    ledger : T.Ledger.t;
    token_metadata : T.token_metadata;
    total_supply : nat;
    operators : T.Operators.t;
  }

end



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


