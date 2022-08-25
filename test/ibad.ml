let test_helloworld () =
  Alcotest.(check int) "" 1 1

let () =
  let open Alcotest in
  run "ibad" [
      "helloworld", [
          test_case "alcotest installed?" `Quick test_helloworld;
        ];
    ]