type status = 
| Ongoing
| Resolved_successful
| Resolved_unsuccessful

type storage = {
    ledger : (address, tez) big_map;
    oracle: address;
    beneficiary: address;
    status: status;
    resolution_date: timestamp
} 

type parameter = 
| Fund
| Get_refund
| Resolve of bool

type return = operation list * storage

let give_refund (sender : address) (_amount : tez) (storage : storage) : return =
    let (amount_to_refund, ledger) = Big_map.get_and_update sender None storage.ledger in
    match amount_to_refund with
    | None -> failwith "there are no funds to refund"  
    | Some amount_to_refund ->
      let destination : unit contract = Tezos.get_contract_with_error sender "sender does not match refund schema" in 
      let storage = {storage with ledger} in
      let tx = Tezos.transaction () amount_to_refund destination in
      [tx], storage

let main (action : parameter) (storage : storage) : return =
  let amount = Tezos.get_amount () in
  let sender = Tezos.get_sender () in
  // short-circuit the entire thing if the beneficiary is not right
  let beneficiary : unit contract = Tezos.get_contract_with_error
    storage.beneficiary
    "beneficiary does not match refund schema"
  in
  match (storage.status, action) with
  | Ongoing, Fund ->
    // prevent folks from sending to the contract if we can't send the money back to them. 
    let _destination : unit contract = Tezos.get_contract_with_error sender "sender does not match refund schema" in
    let prev_funding_amount = Big_map.find_opt sender storage.ledger |> Option.value 0tz in
    let ledger = Big_map.add sender (prev_funding_amount + amount) storage.ledger in
    [], {storage with ledger} 
  | _, Fund -> failwith "Fundraiser is over"
  | Resolved_successful, Get_refund -> failwith "refund no longer possible"
  | Resolved_unsuccessful, Get_refund -> give_refund sender amount storage
  | Ongoing, Get_refund -> give_refund sender amount storage
  | Ongoing, Resolve resolve_status -> 
    let () = if sender = storage.oracle then () else failwith "only oracle can resolve" in
    let () = if Tezos.get_now () > storage.resolution_date then () else failwith "not resolvable yet" in
    if resolve_status then 
      // If successful, send beneficiary all funds 
      let reward = Tezos.get_balance () in
      let tx = Tezos.transaction () reward beneficiary in
      [tx], {storage with status = Resolved_successful}
     else
       [], {storage with status = Resolved_unsuccessful}
  | Resolved_unsuccessful, Resolve _ -> failwith "already resolved" 
  | _, Resolve _ -> failwith "already resolved"
  