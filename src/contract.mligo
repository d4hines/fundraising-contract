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
// Pledgers funds are locked until after the resolution date.
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
// are on a 2 hour delay: first call to %get_refund initiates the timer,
// and the next call after the delay performs the refund.
// After resolution, refunds are always instantaneous.
//
// If the oracle fails to resolve the commitment within 7 days after
// the resolution period, anyone can resolve the fundraiser as
// unsuccessful.
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
} 

type parameter = 
| Commit of timestamp
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
        let timestamp = Tezos.get_now () + 7200 in
        let ledger = Big_map.add sender (amount_to_refund, Some timestamp) ledger in
        [], {storage with ledger} 
    | Funding, Some timestamp -> 
      let () = if Tezos.get_now () > timestamp then () else failwith "you must wait longer before finalizing the withdraw" in
      let storage = {storage with ledger} in
      let tx = Tezos.transaction () amount_to_refund destination in
      [tx], storage
    | Locked _timestamp, _ -> failwith "refund not possible until resolution"
    | Resolved_successful, _ -> failwith "refund no longer possible"
    | Resolved_unsuccessful, _ ->
      // Refunds are instant after the fundraiser is resolved 
      let storage = {storage with ledger} in
      let tx = Tezos.transaction () amount_to_refund destination in
      [tx], storage)
  | Commit resolution_date ->
    (match storage.status with
    | Funding -> [], {storage with status = Locked resolution_date}
    | _ -> failwith "invalid commit")
  | Resolve resolve_status -> 
    (match storage.status with
    | Locked resolution_date ->
      let now = Tezos.get_now () in
      let () = assert_with_error (now > resolution_date) "cannot resolve until after resolution period" in
      let seven_days = 604800 in
      if now > (resolution_date + seven_days) then
        [], {storage with status = Resolved_unsuccessful}
      else
        let () = assert_with_error ((Tezos.get_sender ()) = storage.oracle) "only oracle can resolve until a week after resolution period is over" in
        (if resolve_status then 
          //If successful, send beneficiary all funds 
          let reward = Tezos.get_balance () in
          let tx = Tezos.transaction () reward beneficiary in
          [tx], {storage with status = Resolved_successful}
        else
          [], {storage with status = Resolved_unsuccessful})
    | _ -> failwith "not a resolvable stage")
