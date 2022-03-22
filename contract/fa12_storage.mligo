// A proxy contract for storage that allows for storage upgrades 
#import "fa12_types.mligo" "T"

(* =============================================================================
 * Storage
 * ============================================================================= *)

module Storage = struct
  type token_id = nat
  type t = {
    tzip18 : T.tzip18;
    ledger : T.Ledger.t;
    token_metadata : (nat, T.token_metadata_entry) big_map;
    total_supply : nat;
    allowances : T.allowances;
  }
end