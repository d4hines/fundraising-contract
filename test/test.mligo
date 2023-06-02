#include "../src/contract.mligo"
#include "./test_utils.mligo"


// Scenarios:
let default_init (_pledger1 : address ) (_pledger2 : address) = 
    let ledger : (address, ledger_entry) big_map = Big_map.empty in
    0tz, Funding, ledger

let reset_state init_fn =
    let _ = Test.reset_state  5n ([4000000tz; 100tz; 100tz; 100tez; 100tez;] : tez list) in
    let faucet = Test.nth_bootstrap_account 0 in
    let _ = Test.set_baker faucet in
    let beneficiary : address = Test.nth_bootstrap_account 1 in
    let oracle : address = Test.nth_bootstrap_account 2 in
    let pledger1 : address = Test.nth_bootstrap_account 3 in
    let pledger2 : address = Test.nth_bootstrap_account 4 in
    let (amount, status, ledger) = init_fn pledger1 pledger2 in
    let initial_storage : storage = {
        ledger;
        status;
        oracle;
        beneficiary;
        resolution_period = 3600;
    } in
    let () = Test.set_source faucet in 
    let taddr, _, _ = Test.originate_uncurried main initial_storage amount in 
    (beneficiary, oracle, pledger1, pledger2, taddr) 

let test_get_refund_before_commit () = 
    let (_beneficiary, _oracle, pledger1, pledger2, taddr) = reset_state default_init in
    let original_pledger1_balance = Test.get_balance pledger1 in
    let original_pledger2_balance = Test.get_balance pledger2 in
    // give pledger1's pledge
    let pledger1_amount = 5tz in
    let storage = call_as_exn pledger1 Give_pledge pledger1_amount taddr in
    let (pledger1_pledge_balance, _) = Big_map.find_opt pledger1 storage.ledger |> Option.unopt in
    let () = assert_with_error (pledger1_pledge_balance = pledger1_amount) "pledger1 balance is incorrect" in
    // give pledger2's pledge
    let pledger2_amount = 3tz in
    let storage = call_as_exn pledger2 Give_pledge pledger2_amount taddr in
    let (pledger2_pledge_balance, _) = Big_map.find_opt pledger2 storage.ledger |> Option.unopt in
    let () = assert_with_error (pledger2_pledge_balance = pledger2_amount) "pledger2 balance is incorrect" in 
    // initiate refunds 
    let _storage = call_as_exn pledger1 Get_refund 0tz taddr in
    let _storage = call_as_exn pledger2 Get_refund 0tz taddr in 
    let () = call_as_and_expect "you must wait longer before finalizing the withdraw" pledger1 Get_refund 0tz taddr in
    let () = call_as_and_expect "you must wait longer before finalizing the withdraw" pledger2 Get_refund 0tz taddr in
    let post_transfer_pledger1_balance = Test.get_balance pledger1 in
    let post_transfer_pledger2_balance = Test.get_balance pledger2 in
    let () = Test.bake_until_n_cycle_end 100n in 
    let post_bake_pledger1_balance = Test.get_balance pledger1 in
    let post_bake_pledger2_balance = Test.get_balance pledger2 in 
    let pledger1_baking_rewards = post_bake_pledger1_balance - post_transfer_pledger1_balance |> Option.unopt in 
    let pledger2_baking_rewards = post_bake_pledger2_balance - post_transfer_pledger2_balance |> Option.unopt in 
    let _storage = call_as_exn pledger1 Get_refund 0tz taddr in
    let storage = call_as_exn pledger2 Get_refund 0tz taddr in 
    let () = assert_with_error (Big_map.find_opt pledger1 storage.ledger |> Option.is_none) "pledger1 should have been refunded" in 
    let () = assert_with_error (Big_map.find_opt pledger2 storage.ledger |> Option.is_none) "pledger2 should have been refunded" in 
    let final_pledger1_balance = Test.get_balance pledger1 in
    let final_pledger2_balance = Test.get_balance pledger2 in
    // Each user's final balance should be very close to their original balance plus
    // the baking rewards they received while waiting. 
    // pledger1 diff
    let fees_upper_bound = 3.02tz in // 1tez per tx + gas/storage
    let diff = (original_pledger1_balance + pledger1_baking_rewards) - final_pledger1_balance |> Option.unopt in
    let () = assert_with_error (diff < fees_upper_bound) "user should get all funds back minus tx fees" in
    // pledger2 diff
    let diff = (original_pledger2_balance + pledger2_baking_rewards) - final_pledger2_balance |> Option.unopt in
    let () = assert_with_error (diff < fees_upper_bound) "user should get all funds back minus tx fees" in 
    // contract should hold no tez after all refunds
    let contract_balance = (Test.to_contract taddr |> Tezos.address |> Test.get_balance) in
    let () = assert_equal_poly "contract should have no tez left" contract_balance 0tz in
    ()

let test_beneficiary_gets_tez_after_resolve_true () = 
    let (beneficiary, oracle, pledger1, pledger2, taddr) = reset_state default_init in
    let contract_addr = Test.to_contract taddr |> Tezos.address in
    let pledger1_amount = 5tz in
    let pledger2_amount = 3tz in
    let _storage = call_as_exn pledger1 Give_pledge pledger1_amount taddr in
    let storage = call_as_exn pledger2 Give_pledge pledger2_amount taddr in
    let contract_balance = Test.get_balance contract_addr in 
    let () = assert_equal_poly "contract balance should be sum of pledges" contract_balance 8tz in
    let (pledger1_pledge_balance, _) = Big_map.find_opt pledger1 storage.ledger |> Option.unopt in 
    let () = assert_equal_poly "pledger1 pledge is incorrect" pledger1_pledge_balance 5tz in
    let (pledger2_pledge_balance, _) = Big_map.find_opt pledger2 storage.ledger |> Option.unopt in 
    let () = assert_equal_poly "pledger2 pledge is incorrect" pledger2_pledge_balance 3tz in
    let _storage = call_as_exn beneficiary Commit 0tz taddr in
    let benficiary_pre_resolution_balance = Test.get_balance beneficiary in
    // bake until some time after the resolution date
    let () = Test.bake_until_n_cycle_end 300n in 
    let _storage = call_as_exn oracle (Resolve true) 0tz taddr in
    // contract should hold no tez after resolve true
    let contract_balance = Test.get_balance contract_addr in
    let () = assert_equal_poly "contract should have no tez left" contract_balance 0tz in
    // beneficiary should get all the tez 
    let new_beneficiary_balance = Test.get_balance beneficiary in
    let expected_balance = benficiary_pre_resolution_balance + pledger1_amount + pledger2_amount in
    let () = assert_equal_poly "beneficiary should receive all tez on resolve true" new_beneficiary_balance expected_balance in
    ()
 
let test_get_refund_after_resolution_false () = 
    let (beneficiary, oracle, pledger1, pledger2, taddr) = reset_state default_init in
    let original_pledger1_balance = Test.get_balance pledger1 in 
    let original_pledger2_balance = Test.get_balance pledger2 in 
    // give pledger1's pledge
    let pledger1_amount = 5tz in
    let pledger2_amount = 3tz in
    let _storage = call_as_exn pledger1 Give_pledge pledger1_amount taddr in
    let _storage = call_as_exn pledger2 Give_pledge pledger2_amount taddr in
    let _storage = call_as_exn beneficiary Commit 0tz taddr in
    // bake until some time after the resolution date
    let () = Test.bake_until_n_cycle_end 300n in 
    let _storage = call_as_exn oracle (Resolve false) 0tz taddr in
    let _storage = call_as_exn pledger1 Get_refund 0tz taddr in 
    let _storage = call_as_exn pledger2 Get_refund 0tz taddr in
    let fees_upper_bound = 2.02tz in // 1 tez per tx + gas/storage
    // check pledger1 balance 
    let new_pledger1_balance = Test.get_balance pledger1 in
    let diff = original_pledger1_balance - new_pledger1_balance |> Option.unopt in
    let () = assert_with_error (diff < fees_upper_bound) "user should get all funds back minus tx fees" in 
    // check pledger1 balance 
    let new_pledger2_balance = Test.get_balance pledger2 in
    let diff = original_pledger2_balance - new_pledger2_balance |> Option.unopt in
    let () = assert_with_error (diff < fees_upper_bound) "user should get all funds back minus tx fees" in 
    () 

let test_cant_resolve_before_correct_time () =
    let (beneficiary, oracle, pledger1, _pledger2, taddr) = reset_state default_init in
    let () = call_as_and_expect "cannot resolve the contract before it is locked" oracle (Resolve false) 0tz taddr in
    let () = call_as_and_expect "cannot resolve the contract before it is locked" pledger1 (Resolve false) 0tz taddr in
    let _storage = call_as_exn beneficiary Commit 0tz taddr in
    let () = Test.bake_until_n_cycle_end 100n in 
    let () = call_as_and_expect "only oracle can resolve until a week after the resolution date" pledger1 (Resolve false) 0tz taddr in
    let () = Test.bake_until_n_cycle_end 3360n in 
    let storage = call_as_exn pledger1 (Resolve true) 0tz taddr in
    let () = assert_equal_poly "resolution should be unsuccessful regardless of input after 1 week" storage.status Resolved_unsuccessful in
    ()

// Properties

let test_amount_in_contract_always_equals_sum_of_pledges () =
    let (_beneficiary, _oracle, pledger1, _pledger2, taddr) = reset_state default_init in
    let amount_1 = 5tz in
    let storage = call_as_exn pledger1 Give_pledge amount_1 taddr in
    let (pledger1_pledge_balance, _) = Big_map.find_opt pledger1 storage.ledger |> Option.unopt in
    let () = assert_with_error (pledger1_pledge_balance = amount_1) "pledger1 balance is incorrect" in
    let amount_2 = 3tz in
    let storage = call_as_exn pledger1 Give_pledge amount_2 taddr in
    let (pledger1_pledge_balance, _) = Big_map.find_opt pledger1 storage.ledger |> Option.unopt in
    let () = assert_with_error (pledger1_pledge_balance = amount_1 + amount_2) "pledger1 balance is incorrect" in 
    ()

let test_amount_refunded_always_equals_amount_in_contract () =
    let (_beneficiary, _oracle, pledger1, _pledger2, taddr) =
        reset_state (fun pledger1 _pledger2 ->
            let ledger : (address, ledger_entry) big_map = Big_map.empty in
            let a_long_time_from_now = Tezos.get_now () + 1000000000 in
            let pledger1_entry : ledger_entry = (100tz, Some a_long_time_from_now) in
            let ledger = Big_map.add pledger1 pledger1_entry ledger in
            (200tz, Resolved_unsuccessful, ledger)
        ) in
    let initial_pledger1_balance = Test.get_balance pledger1 in
    let _storage = call_as_exn pledger1 Get_refund 0tz taddr in
    let final_pledger1_balance = Test.get_balance pledger1 in
    let fees_upper_bound = 1.02tz in // 1tez per tx + gas/storage
    let diff = (initial_pledger1_balance + 100tz) - final_pledger1_balance |> Option.unopt in
    let () = assert_with_error (diff < fees_upper_bound) "user should get all funds back minus tx fees" in
    ()

let test_only_beneficiary_can_commit () =
    let (_beneficiary, _oracle, pledger1, _pledger2, taddr) = reset_state default_init in
    let () = call_as_and_expect "only the beneficiary can commit" pledger1 Commit 0tz taddr in
    ()
 
let test_can_only_commit_during_funding () =
    let (beneficiary, _oracle, _pledger1, _pledger2, taddr) = reset_state default_init in
    let _storage = call_as_exn beneficiary Commit 0tz taddr in
    let () = call_as_and_expect "invalid commit" beneficiary Commit 0tz taddr in
    ()

let () = test_get_refund_before_commit ()
let () = test_beneficiary_gets_tez_after_resolve_true ()
let () = test_get_refund_after_resolution_false ()
let () = test_cant_resolve_before_correct_time ()
let () = test_beneficiary_gets_tez_after_resolve_true ()
let () = test_get_refund_after_resolution_false ()
let () = test_cant_resolve_before_correct_time ()
let () = test_amount_in_contract_always_equals_sum_of_pledges ()
let () = test_amount_refunded_always_equals_amount_in_contract ()
let () = test_only_beneficiary_can_commit ()
let () = test_can_only_commit_during_funding ()
