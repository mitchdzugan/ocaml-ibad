module PF = struct
  type ('p, 's, 't, 'a, 'b) optic

  module type PROFUNCTOR = sig
    type ('a, 'b) t

    val dimap : ('a -> 'b) -> ('c -> 'd) -> ('b, 'c) t -> ('a, 'd) t
  end

  module type STRONG = sig
    include PROFUNCTOR

    val first : ('a, 'b) t -> ('a * 'c, 'b * 'c) t

    (*val second : ('a, 'b) t -> ('a * 'b, 'a * 'c) t*)
  end

  module type OPTIC = sig
    type s

    type t

    type a

    type b

    module Mk : functor (P : PROFUNCTOR) -> sig
      val run : (a, b) P.t -> (s, t) P.t
    end
  end

  type ('r, 'a, 'b) forget = Forget of ('a -> 'r)

  type ('a, 'b) func = Func of ('a -> 'b)

  type ('r, 's, 't, 'a, 'b) fold = (('r, 'a, 'b) forget, 's, 't, 'a, 'b) optic

  type ('s, 't, 'a, 'b) agetter = ('a, 'a, 'b) forget -> ('a, 's, 't) forget

  type ('s, 'a) agetter' = ('s, 's, 'a, 'a) agetter

  type ('s, 't, 'a, 'b) asetter = ('a, 'b) func -> ('s, 't) func

  let mkF f = Forget f

  let ofF = function
    | Forget f -> f

  module type TYPE = sig
    type t
  end

  module ForgetStrongProfunctor (T : TYPE) :
    STRONG with type ('a, 'b) t = (T.t, 'a, 'b) forget = struct
    type ('a, 'b) t = (T.t, 'a, 'b) forget

    let dimap f _ fz = mkF @@ fun x -> ofF fz @@ f x

    let first fz = mkF @@ fun x -> ofF fz @@ fst x

    (*let second fz = mkF @@ fun x -> ofF fz @@ snd x*)
  end

  let view (type s t a b)
    (lens :
      (module OPTIC with type s = s and type t = t and type a = a and type b = b)
      )
    =
    let module L = (val lens) in
    let module R = L.Mk (ForgetStrongProfunctor (struct type t = a end)) in
    ofF @@ R.run (Forget (fun x -> x))

  let ( ^@ ) o l = view l o

  let to' f p = mkF (fun z -> ofF p @@ f z)

  let mk_iso
    (iso :
      (module OPTIC
         with type s = 's
          and type t = 't
          and type a = 'a
          and type b = 'b
      )
      )
    =
    iso

  let mk_lens
    (lens :
      (module OPTIC
         with type s = 's
          and type t = 't
          and type a = 'a
          and type b = 'b
      )
      )
    =
    lens

  type testIso = { a : int }

  type testLens = {
    d : int;
    b : int;
  }

  let _a =
    mk_iso
      ( module struct
        type s = testIso

        type t = testIso

        type a = int

        type b = int

        module Mk (P : PROFUNCTOR) = struct
          let run p = P.dimap (fun tt -> tt.a) (fun a -> { a }) p
        end
      end
      )

  let _d =
    mk_lens
      ( module struct
        type s = testLens

        type t = testLens

        type a = int

        type b = int

        module Mk (P : STRONG) = struct
          let run p =
            P.dimap
              (fun tt -> tt.d, fun d -> { d; b = tt.b })
              (fun (b, f) -> f b)
              (P.first p)
        end
      end
      )

  let testIsoObj = { a = 1 }

  let testLensObj = { d = 1; b = 7 }

  let aVal = testIsoObj ^@ _a

  let dVal = testLensObj ^@ _d
end

module VL = struct
  module type FUNCTOR = sig
    type 'a t

    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  module type LENS = sig
    type a

    type b

    module Mk : functor (F : FUNCTOR) -> sig
      val run : (b -> b F.t) -> a -> a F.t
    end
  end

  module IdFunctor : FUNCTOR with type 'a t = 'a = struct
    type 'a t = 'a

    let map f x = f x
  end

  module type TYPE = sig
    type t
  end

  module ConstFunctor (T : TYPE) : FUNCTOR with type 'a t = T.t = struct
    type 'a t = T.t

    let map _ x = x
  end

  type ('a, 'b) lens = (module LENS with type a = 'a and type b = 'b)

  let mk_lens (lens : (module LENS with type a = 'a and type b = 'b))
    : ('a, 'b) lens
    =
    lens

  let modify (type u v) (lens : (module LENS with type a = u and type b = v))
    (f : v -> v) (x : u)
    =
    let module L = (val lens) in
    let module R = L.Mk (IdFunctor) in
    R.run f x

  let set lens x = modify lens (fun _ -> x)

  let view (type u v) (lens : (module LENS with type a = u and type b = v))
    (x : u)
    =
    let module L = (val lens) in
    let module R = L.Mk (ConstFunctor (struct type t = v end)) in
    R.run (fun x -> x) x

  let compose (type u v x) (l1 : (module LENS with type a = v and type b = x))
    (l2 : (module LENS with type a = u and type b = v))
    =
    mk_lens
      ( module struct
        type a = u

        type b = x

        module L1 = (val l1)

        module L2 = (val l2)

        module Mk (F : FUNCTOR) = struct
          module R1 = L1.Mk (F)
          module R2 = L2.Mk (F)

          let run f x = R2.run (R1.run f) x
        end
      end
      )

  let ( // ) a b = compose b a

  let ( ^@ ) o l = view l o

  let ( @% ) l f = modify l f
end