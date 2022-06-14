#import "../src/main.mligo"        "PX"

#import "fa12.mligo"         "F12"
#import "fa12_types.mligo"   "F12T"
#import "fa12_storage.mligo" "F12S"

#import "fa2.mligo"          "F2"
#import "fa2_types.mligo"    "F2T"
#import "fa2_storage.mligo"  "F2S"

// ===== SIGNERS =====
let () = Test.reset_state 6n ([] : tez list)
let alice = Test.nth_bootstrap_account 0
let bob   = Test.nth_bootstrap_account 1
let carol = Test.nth_bootstrap_account 2
let dan   = Test.nth_bootstrap_account 3
let eve   = Test.nth_bootstrap_account 4
let frank = Test.nth_bootstrap_account 5
let () = Test.log ("=========================== TEST USERS ===============================")
let () =  Test.log ("Alice : ", alice)
let () =  Test.log ("Bob   : ", bob)
let () =  Test.log ("Carol : ", carol)
let () =  Test.log ("Dan   : ", dan)
let () =  Test.log ("Eve   : ", eve)
let () =  Test.log ("Frank : ", frank)

// ===== FAILWITH HELPER =======
let assert_string_failure (res : test_exec_result) (expected : string) : unit =
  let expected = Test.eval expected in
  match res with
  | Fail (Rejected (actual,_)) -> assert (Test.michelson_equal actual expected)
  | Fail (Other) -> failwith "contract failed for an unknown reason"
  | Success _gas -> failwith "contract did not failed but was expected to fail"

// ========== DEPLOY CONTRACT HELPER ============
let originate (type s p) (storage: s) (main: (p * s) -> operation list * s) : (p,s) typed_address * p contract =
    let (typed_address, _, _) = Test.originate main storage 0tez in
    typed_address, Test.to_contract typed_address
let originate_ff (type s p) (file_path: string) (mainName : string) (views: string list) (storage: michelson_program) : 
  address * (p,s) typed_address * p contract =
  let (address_contract, code_contract, _) = Test.originate_from_file file_path mainName views storage 0tez in
  let taddress_contract = (Test.cast_address address_contract : (p, s) typed_address) in
  address_contract, taddress_contract, Test.to_contract taddress_contract

// ===================================
// ========== BEGIN TESTS ============
// ===================================
let test_tzip18_should_work =


  let () = Test.log ("=====================================================================") in
  let () = Test.log ("== PROXY CONTRACT ORIGINATED with Alice as gouvernance ==============") in
  let () = Test.log ("=====================================================================") in
  let () = Test.set_source alice in
  let token_info : (string, bytes) map = Map.literal [ 
    ("name" : string)       , (Bytes.pack "Upgradable token");
    ("decimals" : string)   , (Bytes.pack "3");
    ("symbol" : string)     , (Bytes.pack "UT");
    ("description" : string), (Bytes.pack "The upgradable token");
    ("interfaces" : string) , (Bytes.pack "TZIP-007 TZIP-016 TZIP-018");
    ("authors" : string)    , (Bytes.pack "Upgradable Team");
    ("homepage" : string)   , (Bytes.pack "smart-chain.fr");
    ("icon" : string)       , (Bytes.pack "ipfs://QmRPwZSAUkU6nZNor1qoHu4aajPHYpMXrkyZNi8EaNWAmm");
    ("supply" : string)     , (Bytes.pack "2000.000");
    ("mintable" : string)   , (Bytes.pack "false");
  ] in
  let token_metadata = {
    token_id = 0n;
    token_info = token_info;
  } in
  let metadata = Big_map.literal [ 
    ((0n : nat), token_metadata); 
  ] in
  let governance : address = alice in
  let entrypoints : (string, PX.ep) big_map = Big_map.empty in
  let initial_storage_px = {
      governance = governance;
      entrypoints = entrypoints;
      token_metadata = metadata;
  } in
  let initial_storage_lambda = Test.run (fun (x:PX.storage) -> x) initial_storage_px in
  let (address_px, typed_address_px, contract_px) : 
    address * (PX.parameter, PX.storage) typed_address * PX.parameter contract = 
    originate_ff "contract/proxy.mligo" "main" ([] : string list) initial_storage_lambda
  in
  let storage_px = Test.get_storage typed_address_px in
  let () = Test.log ("============", address_px, "============") in
  let () = Test.log ("=====================================================================") in
  let () = Test.log (storage_px.entrypoints) in
  let () = Test.log (storage_px.token_metadata) in





  let () = Test.log ("=====================================================================") in
  let () = Test.log ("== FA12 CONTRACT ORIGINATED with Bob owner of 2 000,0000 tokens =====") in
  let () = Test.log ("=====================================================================") in
  let () = Test.set_source bob in
  let tzip18 : F12T.tzip18 = {
    proxy           = address_px;
    version         = (1n : nat);
    contract_prev   = (None : address option);
    contract_next   = (None : address option);
  } in
  let token_info : (string, bytes) map = Map.literal [ 
    ("name" : string)       , (Bytes.pack "Upgradable token");
    ("decimals" : string)   , (Bytes.pack "3");
    ("symbol" : string)     , (Bytes.pack "UT");
    ("description" : string), (Bytes.pack "The upgradable token");
    ("interfaces" : string) , (Bytes.pack "TZIP-007 TZIP-016 TZIP-018");
    ("authors" : string)    , (Bytes.pack "Upgradable Team");
    ("homepage" : string)   , (Bytes.pack "smart-chain.fr");
    ("icon" : string)       , (Bytes.pack "ipfs://QmRPwZSAUkU6nZNor1qoHu4aajPHYpMXrkyZNi8EaNWAmm");
    ("supply" : string)     , (Bytes.pack "2000.000");
    ("mintable" : string)   , (Bytes.pack "false");
  ] in
  let token_metadata = {
    token_id = 0n;
    token_info = token_info;
  } in
  let metadata = Big_map.literal [ 
    ((0n : nat), token_metadata); 
  ] in
  let total_supply : nat = 2_000_000n in
  let allowances : (F12T.allowance_key, nat) big_map = Big_map.empty in
  let ledger : (address, nat) big_map = Big_map.literal [ 
    ((bob : address), (total_supply : nat));
  ] in
  let initial_storage_f12 = {
    tzip18 = tzip18;
    ledger = ledger;
    token_metadata = metadata;
    total_supply = total_supply;
    allowances = allowances;
  } in
  let initial_storage_lambda = Test.run (fun (x:F12.storage) -> x) initial_storage_f12 in
  let view_list : string list = ["get_balance"; "get_metadata"] in
  let (address_fa12, typed_address_f12, contract_f12) : 
    address * (bytes, F12.storage) typed_address * bytes contract = 
    originate_ff "contract/fa12.mligo" "main" (view_list : string list) initial_storage_lambda in
  let storage_fa12 = Test.get_storage typed_address_f12 in
  let () = Test.log ("============", address_fa12, "============") in
  let () = Test.log ("=====================================================================") in
  let () = Test.log (storage_fa12) in




  let () = Test.log ("=====================================================================") in
  let () = Test.log ("================= PROXY CONTRACT UPGRADED with FA12 by Alice ========") in
  let () = Test.log ("=====================================================================") in
  let () = Test.set_source alice in

  let ep_transfer : PX.ep = {
    method     = "transfer_V1";
    addr       = address_fa12;
    is_view    = false;
  } in
  let op_transfer : PX.ep_operation = {
    name        = "transfer";
    is_removed  = false;
    entrypoint  = Some (ep_transfer);
  } in
  let ep_approve : PX.ep = {
    method     = "approve_V1";
    addr       = address_fa12;
    is_view    = false;
  } in
  let op_approve : PX.ep_operation = {
    name        = "approve";
    is_removed  = false;
    entrypoint  = Some (ep_approve);
  } in

  let no_version_list : PX.change_version option = (None : PX.change_version option) in
  
  let op_list : PX.ep_operation list = [op_transfer; op_approve] in
  let addr_slave    : address = address_fa12 in
  let call_proxy_upgrade : PX.ep_operation list * PX.change_version option * address = (op_list, no_version_list, addr_slave) in

  let tx2 : test_exec_result = Test.transfer_to_contract
    contract_px
    (Upgrade(call_proxy_upgrade))
    0mutez
  in
  let () =  Test.log (tx2) in
  let storage_px = Test.get_storage typed_address_px in
  let () = Test.log (storage_px.entrypoints) in
  let () = Test.log (storage_px.token_metadata) in




  let () = Test.log ("=====================================================================") in
  let () = Test.log ("== FA12 TRANSFER THROUGH PROXY Bob send 500 to Carol ================") in
  let () = Test.log ("=====================================================================") in
  let () = Test.set_source bob in
  let payload_transfer : F12T.transfer = {
    address_from = bob;
    address_to   = carol;
    value        = 500_000n;
  } in
  let call_proxy_transfer : PX.call_contract = {
    entrypoint_name = ("transfer" : string); 
    payload         = Bytes.pack payload_transfer;
  } in

  let tx1 : test_exec_result = Test.transfer_to_contract
    contract_px
    (Call(call_proxy_transfer))
    0mutez
  in
  // let () =  Test.log (tx1) in
  let storage_fa12 = Test.get_storage typed_address_f12 in
  let () = Test.log (storage_fa12) in



  let () = Test.log ("=====================================================================") in
  let () = Test.log ("== FA2 CONTRACT ORIGINATED by Dan with new metadata and ledger empty ") in
  let () = Test.log ("=====================================================================") in
  let () = Test.set_source dan in
  let tzip18 : F2T.tzip18 = {
    proxy           = address_px;
    contract_prev   = (Some (address_fa12) : address option);
    version         = (2n : nat);
    contract_next   = (None : address option);
  } in
  let token_info : (string, bytes) map = Map.literal [ 
    ("name" : string)       , (Bytes.pack "Upgradable token V2");
    ("decimals" : string)   , (Bytes.pack "6");
    ("symbol" : string)     , (Bytes.pack "UT2");
    ("description" : string), (Bytes.pack "The upgradable token version 2");
    ("interfaces" : string) , (Bytes.pack "TZIP-007 TZIP-012 TZIP-016 TZIP-018");
    ("homepage" : string)   , (Bytes.pack "smart-chain.fr");
    ("icon" : string)       , (Bytes.pack "ipfs://QmRPwZSAUkU6nZNor1qoHu4aajPHYpMXrkyZNi8EaNWAmm");
    ("supply" : string)     , (Bytes.pack "2000.000000");
    ("mintable" : string)   , (Bytes.pack "false");
  ] in
  let token_metadata = {
    token_id = 0n;
    token_info = token_info;
  } in
  let metadata = Big_map.literal [ 
    ((0n : nat), token_metadata); 
  ] in
  let total_supply : nat = 2_000_000_000n in
  let operators : (F2T.Operators.owner, F2T.Operators.operator set) big_map = Big_map.empty in
  let ledger : (address, nat) big_map = Big_map.empty in
  let initial_storage_f2 = {
    tzip18 = tzip18;
    ledger = ledger;
    token_metadata = metadata;
    total_supply = total_supply;
    operators = operators;
  } in
  let initial_storage_lambda = Test.run (fun (x:F2.storage) -> x) initial_storage_f2 in
  let view_list : string list = ["get_balance"; "get_metadata"] in
  let (address_fa2, typed_address_f2, contract_f2) : 
    address * (F2.parameter, F2.storage) typed_address * F2.parameter contract = 
    originate_ff "contract/fa2.mligo" "main" view_list initial_storage_lambda in
  let storage_fa2 = Test.get_storage typed_address_f2 in
  let () = Test.log ("============", address_fa2, "============") in
  let () = Test.log ("=====================================================================") in
  let () = Test.log (storage_fa2) in



  let () = Test.log ("=====================================================================") in
  let () = Test.log ("============= PROXY CONTRACT UPGRADED to FA2 by Alice ===============") in
  let () = Test.log ("=====================================================================") in
  let () = Test.set_source alice in

  // Upgrade of the transfer entrypoint
  let ep_transfer : PX.ep = {
    method  = "transfer_V2";
    addr    = address_fa2;
    is_view = false;
  } in
  let op_transfer : PX.ep_operation = {
    name        = "transfer";
    is_removed  = false;
    entrypoint  = Some (ep_transfer);
  } in

  // Upgrade of the approve entrypoint
  let ep_approve : PX.ep = {
    method  = "approve_V2";
    addr    = address_fa2;
    is_view = false;
  } in
  let op_approve : PX.ep_operation = {
    name        = "approve";
    is_removed  = false;
    entrypoint  = Some (ep_approve);
  } in


  let new_version_list : PX.change_version = {
    old = address_fa12;
    new = address_fa2;
  } in

  let op_list : PX.ep_operation list = [op_transfer; op_approve] in
  let addr_slave : address = address_fa2 in
  let call_proxy_upgrade : PX.ep_operation list * PX.change_version option* address = (op_list, Some(new_version_list), addr_slave) in

  let tx2 : test_exec_result = Test.transfer_to_contract
    contract_px
    (Upgrade(call_proxy_upgrade))
    0mutez
  in
  let () =  Test.log (tx2) in
  let storage_px = Test.get_storage typed_address_px in
  let () = Test.log (storage_px.entrypoints) in
  let () = Test.log (storage_px.token_metadata) in


  let () = Test.log ("=====================================================================") in
  let () = Test.log ("== FA2 TRANSFER THROUGH UPGRADED PROXY Bob send again 300 to Carol ==") in
  let () = Test.log ("=====================================================================") in
  let () = Test.set_source bob in

  let atomic_trans : F2T.atomic_trans = {
    to_    = carol;
    amount = 300_000_000n;
  } in

  let atomic_trans_list : F2T.atomic_trans list = [atomic_trans] in

  let transfer_simple : F2T.transfer_from = {
    from_ = bob;
    tx    = atomic_trans_list
  } in

  let payload_transfer : F2T.transfer_from list = [transfer_simple] in

  let call_proxy_transfer : PX.call_contract = {
    entrypoint_name = ("transfer" : string); 
    payload         = Bytes.pack payload_transfer;
  } in

  let tx3 : test_exec_result = Test.transfer_to_contract
    contract_px
    (Call(call_proxy_transfer))
    0mutez
  in
  let () =  Test.log (tx3) in

  let storage_fa2 = Test.get_storage typed_address_f2 in
  let () = Test.log (storage_fa2) in

  let storage_fa12 = Test.get_storage typed_address_f12 in
  let () = Test.log (storage_fa12) in
  
  "OK"



