open Ibad.Lens

type testIso = { a : int }

type testLens = {
  d : int;
  b : int;
}


let testIsoObj = { a = 1 }

let testLensObj = { d = 1; b = 7 }

let _d = mk_lens ( module struct
  type s = testLens
  type t = testLens
  type a = int
  type b = int
  module Mk (P : STRONG) = struct
    let run p = P.dimap (fun tt -> (tt.d, (fun d -> { d; b = tt.b }))) (fun (b, f) -> f b) (P.first p)
  end
end)

let _a = mk_iso ( module struct
  type s = testIso
  type t = testIso
  type a = int
  type b = int
  module Mk (P : PROFUNCTOR) = struct
    let run p = P.dimap (fun tt -> tt.a) (fun a -> { a }) p
  end
end)

let dVal = testLensObj ^* (module struct include (val _d) end)

let aVal = testIsoObj ^* (module struct include (val _a) end)

(****
 * Ideally will be able to write the above like this with ppx
 * 
    let _d = [%lens fun tt -> tt.d, fun tt d -> { tt with d }]
    let _a = [%iso fun tt -> tt.a, fun a -> { a }]
    let dVal = testLensObj ^@ [!_d]
    let aVal = testIsoObj ^@ [$_a]
 *
 *)



let test_lens () =
  Alcotest.(check int) "" 1 aVal;
  Alcotest.(check int) "" 1 dVal

let () =
  Format.printf "%b\n" ((((module struct include (val _d) end) *= 3) testLensObj) = { d = 3; b = 7 });
  Format.printf "%b\n" ((((module struct include (val _a) end) *= 3) testIsoObj) = { a = 3 });
  let open Alcotest in
  run "ibad" [ "basic lens", [ test_case "get/modify/set" `Quick test_lens ] ];