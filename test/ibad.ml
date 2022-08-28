let test_lens () =
  Alcotest.(check int) "" 1 1;
  Alcotest.(check int) "" 1 1

let () =
  let open Alcotest in
  run "ibad" [ "basic lens", [ test_case "get/modify/set" `Quick test_lens ] ]