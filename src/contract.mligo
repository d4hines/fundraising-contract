// Contract summary
// 
// Kickstarter-like fundraising with two-phase commit. 
// A deployed contract represents a proposal by some party
// called the beneficiary to do some work over a length of
// time (called the resolution period) for a reward from the
// community.
//
// The public can pledge money (tez) towards this proposal via
// the [%give_pledge] entrypoint.
//
// At any point, the beneficiary can commit to doing the work.
// Pledgers funds are locked until after the resolution date
// which is the time of the commit plus the resolution period.
//
// At any point after the resolution date, some oracle (ideally
// a credibly-neutral third party), can resolve the commitment as
// successful or unsuccessful.
//
// If successful, all funds are immediately transferred to the beneficiary.
//
// After an unsuccessful resolution, or at any point before the
// beneficiary commits, users can get a refund for their pledge.
//
// To prevent griefing the beneficiary, refunds before commitment
// are on a configurable delay: first call to %get_refund initiates the timer,
// and the next call after the delay performs the refund.
// After resolution, refunds are always instantaneous.
//
// After the resolution date, if the oracle fails to resolve the
// commitment within a period known as the oracle timeout, anyone can resolve
// the fundraiser as unsuccessful.
type status = 
| Funding
| Locked of timestamp
| Resolved_successful
| Resolved_unsuccessful

type ledger_entry = tez * (timestamp option)

type storage = {
    ledger : (address, ledger_entry) big_map;
    oracle: address;
    beneficiary: address;
    status: status;
    refund_lock_period: int; // in seconds 
    resolution_period : int; // in seconds
    oracle_timeout : int; // in seconds
} 

type parameter = 
| Commit
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
    | Funding ->
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
    | Funding, None ->
        // allow withdraws after 2 hours 
        let timestamp = Tezos.get_now () + storage.refund_lock_period in
        let ledger = Big_map.add sender (amount_to_refund, Some timestamp) ledger in
        [], {storage with ledger} 
    | Funding, Some timestamp -> 
      let () = if Tezos.get_now () > timestamp then () else failwith "you must wait longer before finalizing the withdraw" in
      let storage = {storage with ledger} in
      let tx = Tezos.transaction () amount_to_refund destination in
      [tx], storage
    | Locked _timestamp, _ -> failwith "refund not possible until after resolution"
    | Resolved_successful, _ -> failwith "refund no longer possible"
    | Resolved_unsuccessful, _ ->
      // Refunds are instant after the fundraiser is resolved 
      let storage = {storage with ledger} in
      let tx = Tezos.transaction () amount_to_refund destination in
      [tx], storage)
  | Commit ->
    let () = assert_with_error ((Tezos.get_sender ()) = storage.beneficiary) "only the beneficiary can commit" in
    (match storage.status with
    | Funding ->
      let resolution_date = (Tezos.get_now ()) + storage.resolution_period in
      [], {storage with status = Locked resolution_date }
    | _ -> failwith "invalid commit")
  | Resolve resolve_status -> 
    (match storage.status with
    | Locked resolution_date ->
      let now = Tezos.get_now () in
      let () = assert_with_error (now > resolution_date) "cannot resolve until after resolution date" in
      if now > (resolution_date + storage.oracle_timeout) then
        [], {storage with status = Resolved_unsuccessful}
      else
        let () = assert_with_error ((Tezos.get_sender ()) = storage.oracle) "only oracle can resolve until a week after the resolution date" in
        (if resolve_status then 
          // If successful, send beneficiary all funds 
          let reward = Tezos.get_balance () in
          let tx = Tezos.transaction () reward beneficiary in
          [tx], {storage with status = Resolved_successful}
        else
          [], {storage with status = Resolved_unsuccessful})
    | _ -> failwith "cannot resolve the contract before it is locked")

// properties that should hold:
// - Final status of the contract should always eventually be Funding | Resolved_successful | Resolved_unsuccessful
// - if the final status is Resolved_successful, beneficiary should get all the money.
// - if the final status is Funding or Resolved_unsuccessful, users should be able get back all their money
// - only the oracle can resolve successful
// - if we ever hit the Locked stage, the contract always eventually moves to Resolved_successful or Resolved_unsuccessful
//   - No one can resolve before the resolution date
//   - before the resolution date + a week, only the oracle can resolve
//   - after the resolution date + a week, anyone can resolve unsuccessful
// - Everyone should be able to get their money back while in the funding phase (on a 2 hour delay)
//     - if you pledge and the beneficiary commits and you decide you want a refund, tough.
