let assert_equal_poly (type a) (message: string) (left: a) (right: a) =
  let left = Test.to_string left in
  let right = Test.to_string right in
  if left = right then ()
  else
    let full_message =
      message
      ^ ", "
      ^ left
      ^ " is not equal to "
      ^ right
    in
    assert_with_error false full_message
 
 let call_as_exn (caller : address) (params : parameter) (amount : tez) (taddr : (parameter, storage) typed_address) : storage =
    let contract = Test.to_contract taddr in
    let () = Test.set_source caller in
    let _gas_consumed = Test.transfer_to_contract_exn contract params amount in
    Test.get_storage taddr

let call_as (caller : address) (params : parameter) (amount : tez) (taddr : (parameter, storage) typed_address) : test_exec_result =
    let contract = Test.to_contract taddr in
    let () = Test.set_source caller in
    Test.transfer_to_contract contract params amount

let call_as_and_expect (expect : string)
    (caller : address) (params : parameter) (amount : tez) (taddr : (parameter, storage) typed_address) : unit =
    match call_as caller params amount taddr with
    | Success _ -> assert_with_error false "unexpected success"
    | Fail (Rejected (err, _)) ->
        let err = Test.to_string err in
        let expect = "\"" ^ expect ^ "\"" in
        assert_equal_poly "unexpected error" err expect
    | Fail err ->
        let () = Test.log err in
        assert_with_error false "unexpected error"
 
let log_balance name player = 
    let () = Test.log name in
    let balance = Test.get_balance player in
    let () = Test.log balance in
    () 
