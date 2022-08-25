module VL : sig
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

  module IdFunctor : sig
    type 'a t = 'a

    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  module type TYPE = sig
    type t
  end

  module ConstFunctor : functor (T : TYPE) -> sig
    type 'a t = T.t

    val map : ('a -> 'b) -> 'a t -> 'b t
  end

  type ('a, 'b) lens = (module LENS with type a = 'a and type b = 'b)

  val mk_lens : (module LENS with type a = 'a and type b = 'b) -> ('a, 'b) lens

  val modify
    :  (module LENS with type a = 'u and type b = 'v) ->
    ('v -> 'v) ->
    'u ->
    'u IdFunctor.t

  val set
    :  (module LENS with type a = 'a and type b = 'b) ->
    'b ->
    'a ->
    'a IdFunctor.t

  val view : (module LENS with type a = 'u and type b = 'v) -> 'u -> 'v

  val compose
    :  (module LENS with type a = 'v and type b = 'x) ->
    (module LENS with type a = 'u and type b = 'v) ->
    ('u, 'x) lens

  val ( // )
    :  (module LENS with type a = 'a and type b = 'b) ->
    (module LENS with type a = 'b and type b = 'c) ->
    ('a, 'c) lens

  val ( ^@ ) : 'a -> (module LENS with type a = 'a and type b = 'b) -> 'b

  val ( @% )
    :  (module LENS with type a = 'a and type b = 'b) ->
    ('b -> 'b) ->
    'a ->
    'a IdFunctor.t
end
