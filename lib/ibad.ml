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