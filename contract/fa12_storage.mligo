// A proxy contract for storage that allows for storage upgrades 
#import "fa12_types.mligo" "T"

(* =============================================================================
 * Storage
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
    ledger : T.ledger;
    token_metadata : (nat, T.token_metadata_entry) big_map;
    total_supply : nat;
    allowances : T.allowances;
  }
end