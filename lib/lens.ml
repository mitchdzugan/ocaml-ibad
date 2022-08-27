module type PROFUNCTOR = sig
  type ('a, 'b) p

  val dimap : ('a -> 'b) -> ('c -> 'd) -> ('b, 'c) p -> ('a, 'd) p
end

module type STRONG = sig
  include PROFUNCTOR

  val first : ('a, 'b) p -> ('a * 'c, 'b * 'c) p

  val second : ('b, 'c) p -> ('a * 'b, 'a * 'c) p
end

module Profunctable (P: PROFUNCTOR) = struct
  type ('a, 'b) p = ('a, 'b) P.p
  let dimap = P.dimap
end

type ('r, 'a, 'b) forget = Forget of ('a -> 'r)
let mkF f = Forget f
let ofF = function | Forget f -> f

module type TYPE = sig
  type t
end

module ForgetStrongProfunctor (T : TYPE) :
  STRONG with type ('a, 'b) p = (T.t, 'a, 'b) forget = struct
  type ('a, 'b) p = (T.t, 'a, 'b) forget

  let dimap f _ fz = mkF @@ fun x -> ofF fz @@ f x

  let first fz = mkF @@ fun x -> ofF fz @@ fst x

  let second fz = mkF @@ fun x -> ofF fz @@ snd x
end

module type OPTIC = sig
  type s
  type t
  type a
  type b
  module Mk : functor (P : PROFUNCTOR) -> sig
    val run : (a, b) P.p -> (s, t) P.p
  end
end

module type STRONG_OPTIC = sig
  include OPTIC
  module Mk : functor (P : STRONG) -> sig
    val run : (a, b) P.p -> (s, t) P.p
  end
end


let view (type s t a b)
  (lens :
    (module STRONG_OPTIC with type s = s and type t = t and type a = a and type b = b)
    )
  =
  let module L = (val lens) in
  let module R = L.Mk (ForgetStrongProfunctor (struct type t = a end)) in
  ofF @@ R.run (Forget (fun x -> x))

let ( ^@ ) o l = view l o

let to' f p = mkF (fun z -> ofF p @@ f z)

type testIso = { a : int }

type testLens = {
  d : int;
  b : int;
}


let testIsoObj = { a = 1 }

let testLensObj = { d = 1; b = 7 }

module D_ = struct
  type s = testLens
  type t = testLens
  type a = int
  type b = int
  module Mk (P : STRONG) = struct
    let run p = P.dimap (fun tt -> (tt.d, (fun d -> { d; b = tt.b }))) (fun (b, f) -> f b) (P.first p)
  end
end

module A_ = struct
  type s = testIso
  type t = testIso
  type a = int
  type b = int
  module Mk (P : PROFUNCTOR) = struct
    let run p = P.dimap (fun tt -> tt.a) (fun a -> { a }) p
  end
end

let dVal = testLensObj ^@ (module struct include D_ end)

let aVal = testIsoObj ^@ (module struct include A_ end)

(****
 * Ideally will be able to write the above like this with ppx
 * 
    module D_ = [%lens fun tt -> tt.d, fun tt d -> { tt with d }]
    module A_ = [%iso fun tt -> tt.a, fun a -> { a }]
    let dVal = testLensObj ^@ [D_]
    let aVal = testIsoObj ^@ [A_]
 *
 *)
