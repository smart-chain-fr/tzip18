type tzip18 = {
  contract_old : address option;
  version_old : nat option;
  version_current : nat;
  contract_next : address option;
  version_next : nat option;
  is_in_use : bool;
}

type type_ligo = Na | In | Ad | St

module Ledger  = struct
  type owner   = address
  type amount_ = nat
  type t = (owner, amount_) big_map
end

type token_metadata = { 
  token_id: nat; 
  token_info: (string, bytes) map;
}

type approve = { 
  spender : address; 
  value : nat;
}

type allowance_key = { 
  owner : address;
  spender : address;
}

type allowances = (allowance_key, nat) big_map

type transfer = { 
  address_from : address; 
  address_to : address; 
  value : nat;
}

type get_allowance = { 
  request : allowance_key; 
  callback : nat contract;
}

type get_balance = { 
  owner : address; 
  callback : nat contract;
}

type get_total_supply = { 
  request : unit ; 
  callback : nat contract
}