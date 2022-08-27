open Ibad.Lens

type testIso = { a : int }

type testLens = {
  d : int;
  b : int;
}

type testCompose = {
  x: int;
  i: testIso
}

type testPrism =
  | PrismO of int
  | PrismNo of string

let tpgm = function
  | PrismO (n) -> Some(n)
  | _ -> None

let maybe def f = function
 | Some(v) -> f(v)
 | _ -> def

let either fl fr = function
| Either.Left(l) -> fl(l)
| Either.Right(r) -> fr(r)

let _d = mk_lens ( module struct
  type s = testLens
  type t = testLens
  type a = int
  type b = int
  module Mk (P : STRONG) = struct
    let run p = P.dimap (fun tt -> (tt.d, (fun d -> { d; b = tt.b }))) (fun (b, f) -> f b) (P.first p)
  end
end)

let _i = mk_lens ( module struct
  type s = testCompose
  type t = testCompose
  type a = testIso
  type b = testIso
  module Mk (P : STRONG) = struct
    let run p = P.dimap (fun tc -> (tc.i, (fun i -> { i; x = tc.x }))) (fun (b, f) -> f b) (P.first p)
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



let _ia = compose_lens (module struct include (val _a) end)  
                       (module struct include (val _i) end)

let _o = mk_prism ( module struct
  type s = testPrism
  type t = testPrism
  type a = int
  type b = int
  module Mk (P: CHOICE) = struct
    let run p = P.dimap (fun s -> maybe (Either.Left s) (fun r -> Either.Right r) (tpgm s))
                        (either (fun x -> x) (fun x -> x))
                        (P.right (P.dimap (fun x -> x) (fun x -> PrismO x) p))
  end
end)

(****
 * Ideally will be able to write the above like this with ppx
 * 
    let _d = [%lens fun tt -> tt.d, fun tt d -> { tt with d }]
    let _a = [%iso fun tt -> tt.a, fun a -> { a }]
    let dVal = testLensObj ^@ [!_d]
    let aVal = testIsoObj ^@ [$_a]
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
  Alcotest.(check int) "" 3 cVal

let () =
  Format.printf "%b\n" (((module struct include (val _d) end) *= 3 @@ testLensObj) = { d = 3; b = 7 });
  Format.printf "%b\n" (((module struct include (val _a) end) *= 3 @@ testIsoObj) = { a = 3 });
  Format.printf "%b\n" (((module struct include (val _o) end) *= 3 @@ testPrismObj1) = PrismO 3);
  Format.printf "%b\n" (((module struct include (val _o) end) *= 3 @@ testPrismObj2) = testPrismObj2);
  let open Alcotest in
  run "ibad" [ "basic lens", [ test_case "get/modify/set" `Quick test_lens ] ];