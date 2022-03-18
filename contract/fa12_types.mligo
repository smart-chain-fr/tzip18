

type ledger = (address, nat) big_map

type token_metadata_entry = { token_id: nat; token_info: (string, bytes) map;}

type approve = { spender : address; value : nat }

type allowance_key = { owner : address ; spender : address }

type allowances = (allowance_key, nat) big_map

type transfer = { address_from : address; address_to : address; value : nat }

type getAllowance = { request : allowance_key; callback : nat contract }

type getBalance = { owner : address; callback : nat contract }

type getTotalSupply = { request : unit ; callback : nat contract }