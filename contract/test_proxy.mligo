#import "proxy.mligo"        "PX"

#import "fa12.mligo"         "F12"
#import "fa12_types.mligo"   "F12T"
#import "fa12_storage.mligo" "F12S"

#import "fa2.mligo"          "F2"
#import "fa2_types.mligo"    "F2T"
#import "fa2_storage.mligo"  "F2S"

// ===== SIGNERS =====
let _create_accounts = Test.reset_state 6n ([] : tez list)
let alice = Test.nth_bootstrap_account 0
let bob   = Test.nth_bootstrap_account 1
let carol = Test.nth_bootstrap_account 2
let dan   = Test.nth_bootstrap_account 3
let eve   = Test.nth_bootstrap_account 4
let frank = Test.nth_bootstrap_account 5

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
  let (address_contract, code_contract, _) = Test.originate_from_file file_path mainName storage 0tez in
  let taddress_contract = (Test.cast_address address_contract : (p, s) typed_address) in
  address_contract, taddress_contract, Test.to_contract taddress_contract

// ===================================
// ========== BEGIN TESTS ============
// ===================================
let test_create_tzip18_fa12_should_work =



  let () = Test.log ("=====================================================================") in
  let () = Test.log ("== FA12 CONTRACT ORIGINATED with Bob owner of 2 000,0000 tokens =====") in
  let () = Test.log ("=====================================================================") in
  let tzip18 : F12T.tzip18 = {
    contract_old    = (None : address option);
    version_old     = (None : nat option);
    version_current = 1n;
    contract_next   = (None : address option);
    version_next    = (None : nat option);
    is_in_use       = false;
  } in
  let token_info : (string, bytes) map = Map.literal [ 
    ("name" : string)       , (Bytes.pack "Upgradable token");
    ("decimals" : string)   , (Bytes.pack "3");
    ("symbol" : string)     , (Bytes.pack "UT");
    ("description" : string), (Bytes.pack "The upgradable token");
    ("interfaces" : string) , (Bytes.pack "TZIP-007 TZIP-016");
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
  let total_supply : nat = 2000000n in
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
  let (address_fa12, typed_address_f12, contract_f12) : 
    address * (F12.parameter, F12.storage) typed_address * F12.parameter contract = 
    originate_ff "contract/fa12.mligo" "main" ([] : string list) initial_storage_lambda in
  let storage_fa12 = Test.get_storage typed_address_f12 in
  let () = Test.log ("============", address_fa12, "============") in
  let () = Test.log ("=====================================================================") in
  let () = Test.log (storage_fa12) in



  let () = Test.log ("=====================================================================") in
  let () = Test.log ("== PROXY CONTRACT ORIGINATED with Alice as gouvernance ==============") in
  let () = Test.log ("=====================================================================") in
  let ep_transfer : PX.ep = {
    addr       = address_fa12;
    is_view    = false;
  } in
  let ep_approve : PX.ep = {
    addr       = address_fa12;
    is_view    = false;
  } in
  let governance_proxy : address = alice in
  let entrypoints : (string, PX.ep) big_map = Big_map.literal [ 
    (("%transfer" : string), (ep_transfer : PX.ep));
    (("%approve"  : string), (ep_approve  : PX.ep)); 
  ] in
  let initial_storage_px = {
      governance_proxy = governance_proxy;
      entrypoints = entrypoints;
      token_metadata = storage_fa12.token_metadata;
  } in
  let initial_storage_lambda = Test.run (fun (x:PX.storage) -> x) initial_storage_px in
  let (address_px, typed_address_px, contract_px) : 
    address * (PX.parameter, PX.storage) typed_address * PX.parameter contract = 
    originate_ff "contract/proxy.mligo" "main" ([] : string list) initial_storage_lambda
  in
  let storage_px = Test.get_storage typed_address_px in
  let () = Test.log ("============", address_px, "============") in
  let () = Test.log ("=====================================================================") in
  let () = Test.log (storage_px) in
  let () = Test.log (storage_px.entrypoints) in
  let () = Test.log (storage_px.token_metadata) in



  let () = Test.log ("=====================================================================") in
  let () = Test.log ("== FA12 TRANSFER THROUGH PROXY Bob send 500 to Carol ================") in
  let () = Test.log ("=====================================================================") in
  let () = Test.set_source bob in
  let payload_transfer : F12T.transfer = {
    address_from = bob;
    address_to   = carol;
    value        = 500000n;
  } in
  let call_proxy_transfer : PX.call_contract = {
    entrypoint_name = ("%transfer" : string); 
    payload         = Bytes.pack payload_transfer;
  } in
  let tx1 : test_exec_result = Test.transfer_to_contract
    contract_px
    (CallContract(call_proxy_transfer))
    0mutez
  in
  let storage_fa12 = Test.get_storage typed_address_f12 in
  let () = Test.log (storage_fa12) in



  // let () = Test.log ("=====================================================================") in
  // let () = Test.log ("== FA2 CONTRACT ORIGINATED by Dan with the storage of the FA12 ======") in
  // let () = Test.log ("=====================================================================") in
  // let () = Test.set_source dan in
  // let tzip18 : F2T.tzip18 = {
  //   contract_old    = (address_fa12 : address option);
  //   version_old     = (1n : nat option);
  //   version_current = (2n : nat);
  //   contract_next   = (None : address option);
  //   version_next    = (None : nat option);
  //   is_in_use       = false;
  // } in
  // let token_info : (string, bytes) map = Map.literal [ 
  //   ("name" : string)       , (Bytes.pack "Upgradable token");
  //   ("decimals" : string)   , (Bytes.pack "3");
  //   ("symbol" : string)     , (Bytes.pack "UT");
  //   ("description" : string), (Bytes.pack "The upgradable token");
  //   ("interfaces" : string) , (Bytes.pack "TZIP-007 TZIP-016");
  //   ("authors" : string)    , (Bytes.pack "Upgradable Team");
  //   ("homepage" : string)   , (Bytes.pack "smart-chain.fr");
  //   ("icon" : string)       , (Bytes.pack "ipfs://QmRPwZSAUkU6nZNor1qoHu4aajPHYpMXrkyZNi8EaNWAmm");
  //   ("supply" : string)     , (Bytes.pack "2000.000");
  //   ("mintable" : string)   , (Bytes.pack "false");
  // ] in
  // let token_metadata = {
  //   token_id = 0n;
  //   token_info = token_info;
  // } in
  // let metadata = Big_map.literal [ 
  //   ((0n : nat), token_metadata); 
  // ] in
  // let total_supply : nat = 2000000n in
  // let allowances : (F12T.allowance_key, nat) big_map = Big_map.empty in
  // let ledger : (address, nat) big_map = Big_map.literal [ 
  //   ((bob : address), (total_supply : nat));
  // ] in
  // let initial_storage_f12 = {
  //   tzip18 = tzip18;
  //   ledger = ledger;
  //   token_metadata = metadata;
  //   total_supply = total_supply;
  //   allowances = allowances;
  // } in
  // let initial_storage_lambda = Test.run (fun (x:F12.storage) -> x) initial_storage_f12 in
  // let (address_fa12, typed_address_f12, contract_f12) : 
  //   address * (F12.parameter, F12.storage) typed_address * F12.parameter contract = 
  //   originate_ff "contract/fa12.mligo" "main" ([] : string list) initial_storage_lambda in
  // let storage_fa12 = Test.get_storage typed_address_f12 in
  // let () = Test.log ("============", address_fa12, "============") in
  // let () = Test.log ("=====================================================================") in
  // let () = Test.log (storage_fa12) in





  "OK"


  // let () = Test.log ("=====================================================================") in
  // let () = Test.log ("== PROXY CONTRACT UPGRADED by Alice =================================") in
  // let () = Test.log ("=====================================================================") in

