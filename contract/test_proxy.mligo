#import "proxy.mligo"        "PX"

#import "fa12.mligo"         "F12"
#import "fa12_types.mligo"   "F12T"
#import "fa12_storage.mligo" "F12S"

#import "fa2.mligo"          "F2"
#import "fa2_types.mligo"    "F2T"
#import "fa2_storage.mligo"  "F2S"

// ===== SIGNERS =====
let _create_accounts = Test.reset_state 6n ([] : tez list)
let alice   = Test.nth_bootstrap_account 0
let bob     = Test.nth_bootstrap_account 1
let charly  = Test.nth_bootstrap_account 2
let delta   = Test.nth_bootstrap_account 3
let echo    = Test.nth_bootstrap_account 4
let unknown = Test.nth_bootstrap_account 5

// ===== FAILWITH HELPER =======
let assert_string_failure (res : test_exec_result) (expected : string) : unit =
    let expected = Test.eval expected in
    match res with
    | Fail (Rejected (actual,_)) -> assert (Test.michelson_equal actual expected)
    | Fail (Other) -> failwith "Contract have failed"
    | Success -> failwith "Test should have failed"

// ========== DEPLOY CONTRACT HELPER ============
let originate (type s p) (storage: s) (main: (p * s) -> operation list * s) : (p,s) typed_address * p contract =
    let (typed_address, _, _) = Test.originate main storage 0tez in
    typed_address, Test.to_contract typed_address

// ===================================
// ========== BEGIN TESTS ============
// ===================================
let test_create_tzip18_fa12_should_work =
    // Prepare data for initial storage
    let empty_bigmap : (nat, Storage.Types.proposal) big_map = Big_map.empty in
    let tzip18 : F12T.tzip18 = {
      master_proxy    = alice;
      contract_old    = alice;
      version_old     = None;
      version_current = 1;
      contract_next   = None;
      version_next    = None;
      is_in_use       = false;
    } in
    let ledger : F12T.Ledger.t = (address, nat) big_map = Big_map.empty in
    let token_metadata : F12T.token_metadata = { 
      token_id   = 0; 
      token_info = (string, bytes) map;
    } in
    let total_supply : F12T.total_supply = 2000000 in
    let allowances : (allowance_key, nat) big_map = Big_map.empty in
    let initial_storage_fa12 : F12S.Storage.t = (0,{
        tzip18 = tzip18;
        ledger = ledger;
        token_metadata = token_metadata;
        total_supply = total_supply;
        allowances = allowances;
    }) in
    // Originate contract multisig 
    let (_,contract_fa12) = originate initial_storage_fa12 F12.main in
    // Create the initial fa12 storage
    let empty_bigmap : (FA12_Parameter.Types.allowance_key, nat) big_map = Big_map.empty in 
    let initial_storage_fa12 = {
        tokens = Big_map.add alice 1000n Big_map.empty;
        allowances = empty_bigmap;
        total_supply = 1000n;
    } in
    // Originate contract fa12 
    let (typed_address_fa12,contract_fa12) = originate initial_storage_fa12 FA12.main in
    // // Create a new proposal
    // let new_proposal : Parameter.Types.proposal_params = {
    //     target_fa12 = Tezos.address contract_fa12;
    //     target_to = echo;
    //     token_amount = 100n;
    // } in
    // // Send a new proposal without being a signer should fail
    // let () = Test.set_source echo in
    // let fail_tx : test_exec_result = Test.transfer_to_contract contract_multisig (Create_proposal(new_proposal)) 0mutez in
    // let () = assert_string_failure (fail_tx: test_exec_result) ("Only one of the contract signer can create an proposal":string) in
    "✓"

// let test_create_multisig_proposal_with_a_signer_should_work =
//     // Prepare data for initial storage
//     let empty_bigmap : (nat, Storage.Types.proposal) big_map = Big_map.empty in 
//     // Create the initial multisig storage with specific values
//     let initial_storage_multisig : Storage.Types.t = {
//         proposal_counter = 0n;
//         proposal_map = empty_bigmap;
//         signers = (Set.add alice (Set.add bob (Set.add charly (Set.empty : address set))));
//         threshold = 2n;
//     } in
//     // Originate contract multisig and cast a typed address to get the contract artefact
//     let (typed_address_multi,contract_multisig) = originate initial_storage_multisig Multisign.main in
//     // Create the initial fa12 storage
//     let empty_bigmap : (FA12_Parameter.Types.allowance_key, nat) big_map = Big_map.empty in 
//     let initial_storage_fa12 : FA12_Storage.Types.t = {
//         tokens = Big_map.add alice 1000n Big_map.empty;
//         allowances = empty_bigmap;
//         total_supply = 1000n;
//     } in
//     // Originate contract fa12 and cast a typed address to get the contract artefact
//     let (typed_address_fa,contract_fa12) = originate initial_storage_fa12 FA12.main in
//     // Create a new proposal
//     let new_proposal : Parameter.Types.proposal_params = {
//         target_fa12 = Tezos.address contract_fa12;
//         target_to = echo;
//         token_amount = 100n;
//     } in
//     // Send a new proposal with a signer should work
//     let () = Test.set_source alice in
//     let tx : test_exec_result = Test.transfer_to_contract contract_multisig (Create_proposal(new_proposal)) 0mutez in
//     let () = Test.log(tx) in
//     let new_storage : Storage.Types.t = Test.get_storage typed_address_multi in
//     let proposal : Storage.Types.proposal = match Map.find_opt 1n new_storage.proposal_map with
//         Some value -> value
//       | None -> failwith "f"
//     in
//     let () = assert ( proposal.approved_signers = (Set.add alice (Set.empty : address set)) ) in
//     let () = assert ( proposal.executed = false) in
//     let () = assert ( proposal.number_of_signer = 1n) in
//     let () = assert ( proposal.target_fa12 = new_proposal.target_fa12) in
//     let () = assert ( proposal.target_to = echo) in
//     //let () = assert ( proposal.timestamp = (Tezos.now : timestamp)) in
//     let () = assert ( proposal.token_amount = 100n) in
//     "✓"

// let test_sign_a_multisig_proposal_should_work =
//     // Prepare data for initial storage
//     let empty_bigmap : (nat, Storage.Types.proposal) big_map = Big_map.empty in 
//     let address_1 : address = alice in
//     let address_2 : address = bob in
//     let address_3 : address = charly in
//     // Create the initial multisig storage with specific values
//     let initial_storage_multisig : Storage.Types.t = {
//         proposal_counter = 0n;
//         proposal_map = empty_bigmap;
//         signers = (Set.add address_1 (Set.add address_2 (Set.add address_3 (Set.empty : address set))));
//         threshold = 2n;
//     } in
//     // Originate contract multisig and cast a typed address to get the contract artefact
//     let (typed_address_multi,contract_multisig) = originate initial_storage_multisig Multisign.main in
//     // Create the initial fa12 storage
//     let empty_bigmap : (FA12_Parameter.Types.allowance_key, nat) big_map = Big_map.empty in 
//     let initial_storage_fa12 : FA12_Storage.Types.t = {
//         tokens = Big_map.add alice 1000n Big_map.empty;
//         allowances = empty_bigmap;
//         total_supply = 1000n;
//     } in
//     // Originate contract fa12 and cast a typed address to get the contract artefact
//     let (typed_address_fa,contract_fa12) = originate initial_storage_fa12 FA12.main in
//     // Create a new proposal
//     let new_proposal : Parameter.Types.proposal_params = {
//         target_fa12 = Tezos.address contract_fa12;
//         target_to = echo;
//         token_amount = 100n;
//     } in
//     // Send a new proposal with a signer should work
//     let () = Test.set_source alice in
//     let tx : test_exec_result = Test.transfer_to_contract contract_multisig (Create_proposal(new_proposal)) 0mutez in
//     let () = Test.set_source bob in
//     let tx2 : test_exec_result = Test.transfer_to_contract contract_multisig (Sign_proposal(1n)) 0mutez in
 
//     let new_storage : Storage.Types.t = Test.get_storage typed_address_multi in
//     let proposal : Storage.Types.proposal = match Map.find_opt 1n new_storage.proposal_map with
//         Some value -> value
//       | None -> failwith "f"
//     in

//     //let () = assert ( proposal.approved_signers = (Set.add bob (Set.add alice (Set.empty : address set)))) in
//     //let () = assert ( proposal.executed = true) in
//     //let () = assert ( proposal.number_of_signer = 2n) in
//     let () = assert ( proposal.target_fa12 = new_proposal.target_fa12) in
//     let () = assert ( proposal.target_to = echo) in
//     //let () = assert ( proposal.timestamp = (Tezos.now : timestamp)) in
//     let () = assert ( proposal.token_amount = 100n) in
//     "✓"



    