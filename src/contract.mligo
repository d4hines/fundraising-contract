// Contract summary
// 
// Kickstarter-like fundraising with two-phase commit. 
// A deployed contract represents some commitment by a beneficiary.
//
// The public can pledge money (tez) towards this commitment via
// the [%give_pledge] entrypoint.
//
// At any point after the [%resolution_date], some oracle (ideally
// a credibly-neutral third party), can resolve the fundraiser as
// successful or unsuccessful.
//
// If successful, all funds are immediately transferred to the beneficiary.
//
// After an unsuccessful resolution, or at any point before the
// resolution date, users can get a refund for their pledge.
//
// To prevent griefing the beneficiary, refunds before the resolution
// are on a 2 hour delay: first call to %get_refund initiates the timer,
// and the next call after the delay performs the refund.
// After resolution, refunds are always instantaneous.

type status = 
| Ongoing
| Resolved_successful
| Resolved_unsuccessful

type ledger_entry = tez * (timestamp option)

type storage = {
    ledger : (address, ledger_entry) big_map;
    oracle: address;
    beneficiary: address;
    status: status;
    resolution_date: timestamp
} 

type parameter = 
| Give_pledge
| Get_refund
| Resolve of bool

let main (action, storage : parameter * storage) : operation list * storage =
  let amount = Tezos.get_amount () in
  let sender = Tezos.get_sender () in
  // short-circuit the entire thing if the beneficiary is not right
  let beneficiary : unit contract = Tezos.get_contract_with_error
    storage.beneficiary
    "beneficiary does not match refund schema"
  in
  match action with
  | Give_pledge ->
    (match storage.status with 
    | Ongoing ->
      // prevent folks from sending to the contract if we can't send the money back to them. 
      let _destination : unit contract = Tezos.get_contract_with_error sender "sender does not match refund schema" in
      let (prev_funding_amount, _) = Big_map.find_opt sender storage.ledger |> Option.value (0tz, None) in
      let ledger = Big_map.add sender (prev_funding_amount + amount, None) storage.ledger in
      [], {storage with ledger}
    | _ -> failwith "fundraising is over")
  | Get_refund -> 
    // get caller's ledger entry and remove it from the ledger
    let (entry, ledger) : (ledger_entry option * (address, ledger_entry) big_map) = Big_map.get_and_update sender None storage.ledger in
    let (amount_to_refund, request_timestamp) = Option.unopt_with_error entry "there is no pledge to refund" in
    let destination : unit contract = Tezos.get_contract_with_error sender "unreachable" in // impossible to deposit from account with bad schemea
    (match storage.status, request_timestamp with 
    | Ongoing, None ->
        // allow withdraws after 2 hours 
        let timestamp = Tezos.get_now () + 7200 in
        let ledger = Big_map.add sender (amount_to_refund, Some timestamp) ledger in
        [], {storage with ledger} 
    | Ongoing, Some timestamp -> 
      let () = if Tezos.get_now () > timestamp then () else failwith "you must wait longer before finalizing the withdraw" in
      let storage = {storage with ledger} in
      let tx = Tezos.transaction () amount_to_refund destination in
      [tx], storage
    | Resolved_successful, _ -> failwith "refund no longer possible"
    | Resolved_unsuccessful, _ ->
      // Refunds are instant after the fundraiser is resolved 
      let storage = {storage with ledger} in
      let tx = Tezos.transaction () amount_to_refund destination in
      [tx], storage)
  | Resolve resolve_status ->
    // prevent oracle from catching pledgers off-guard and locking
    // their funds.
    if Tezos.get_now () < storage.resolution_date then
      failwith "cannot resolve before resolution date"
    else
    (match storage.status with 
    | Ongoing ->
     if resolve_status then 
      // If successful, send beneficiary all funds 
      let reward = Tezos.get_balance () in
      let tx = Tezos.transaction () reward beneficiary in
      [tx], {storage with status = Resolved_successful}
     else
       [], {storage with status = Resolved_unsuccessful}    
    | _ -> failwith "already resolved") 
