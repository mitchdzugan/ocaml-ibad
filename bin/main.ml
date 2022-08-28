open Ibad.Lens

type testIso = { a : int }

type testLens = {
  d : int;
  b : int;
}

type testCompose = {
  x : int;
  i : testIso;
}

type testPrism =
  | PrismO of int
  | PrismNo of string

let _d = lens (fun tt -> tt.d) (fun tt d -> { tt with d })

let _i = lens (fun tc -> tc.i) (fun tc i -> { tc with i })

let _a = iso (fun ti -> ti.a) (fun a -> { a })

let _ia =
  Compose.Lens.(
    (module struct include (val _i) end) >> (module struct include (val _a) end)
  )

let _o =
  let tpgm = function
    | PrismO n -> Some n
    | _ -> None
  in
  prism (fun x -> PrismO x) tpgm

(****
 * Ideally will be able to write the above like this with ppx
 * 
    let dVal = testLensObj ^@ [% _d]
    let aVal = testIsoObj ^@ [% _a]
 *
 *)

let testIsoObj = { a = 1 }

let testLensObj = { d = 1; b = 7 }

let testPrismObj1 = PrismO 4

let testPrismObj2 = PrismNo ""

let testComposeObj = { x = 1; i = { a = 3 } }

let test_lens () =
  let dVal = testLensObj ^* (module struct include (val _d) end) in
  let aVal = testIsoObj ^* (module struct include (val _a) end) in
  let cVal = testComposeObj ^* (module struct include (val _ia) end) in
  Alcotest.(check int) "" 1 aVal;
  Alcotest.(check int) "" 1 dVal;
  Alcotest.(check int) "" 3 cVal;
  Alcotest.(check int) "" 1 testComposeObj.x;
  Alcotest.(check int) "" 7 testLensObj.b

let () =
  Format.printf "%b\n"
    (((module struct include (val _d) end) *= 3) @@ testLensObj
    = { d = 3; b = 7 }
    );
  Format.printf "%b\n"
    (((module struct include (val _a) end) *= 3) @@ testIsoObj = { a = 3 });
  Format.printf "%b\n"
    (((module struct include (val _o) end) *= 3) @@ testPrismObj1 = PrismO 3);
  Format.printf "%b\n"
    (((module struct include (val _o) end) *= 3) @@ testPrismObj2
    = testPrismObj2
    );
  let open Alcotest in
  run "ibad" [ "basic lens", [ test_case "get/modify/set" `Quick test_lens ] ]
