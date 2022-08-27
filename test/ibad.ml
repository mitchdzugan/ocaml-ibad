let test_lens () =
  Alcotest.(check int) "" 1 Ibad.Lens.aVal;
  Alcotest.(check int) "" 1 Ibad.Lens.dVal

let () =
  let open Alcotest in
  run "ibad" [ "basic lens", [ test_case "get/modify/set" `Quick test_lens ] ]