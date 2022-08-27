let (<<) f g x = f(g(x))
let (>>) g f x = f(g(x))

module type PROFUNCTOR = sig
  type ('a, 'b) p
  val dimap : ('a -> 'b) -> ('c -> 'd) -> ('b, 'c) p -> ('a, 'd) p
end

module type CHOICE = sig
  include PROFUNCTOR
  val left : ('a, 'b) p -> (('a, 'c) Either.t, ('b, 'c) Either.t) p
  val right : ('b, 'c) p -> (('a, 'b) Either.t, ('a, 'c) Either.t) p
end

module type STRONG = sig
  include PROFUNCTOR
  val first : ('a, 'b) p -> ('a * 'c, 'b * 'c) p
  val second : ('b, 'c) p -> ('a * 'b, 'a * 'c) p
end

module type STRONGCHOICE = sig
  include STRONG
  include CHOICE with type ('a, 'b) p := ('a, 'b) p
end

module Profunctable (P: PROFUNCTOR) = struct
  type ('a, 'b) p = ('a, 'b) P.p
  let dimap = P.dimap
end

module type TYPE = sig
  type t
end

(***********************  FORGET  ********************************)
type ('r, 'a, 'b) forget = Forget of ('a -> 'r)
let mkF f = Forget f
let ofF = function | Forget f -> f

module ForgetF (T : TYPE) :
  STRONG with type ('a, 'b) p = (T.t, 'a, 'b) forget = struct
  type ('a, 'b) p = (T.t, 'a, 'b) forget

  let dimap f _ fz = mkF @@ fun x -> ofF fz @@ f x

  let first fz = mkF @@ fun x -> ofF fz @@ fst x

  let second fz = mkF @@ fun x -> ofF fz @@ snd x
end

(***********************  FUNC  ********************************)
type ('a, 'b) func = 'a -> 'b

module Func : STRONGCHOICE with type ('a, 'b) p = ('a, 'b) func = struct
  type ('a, 'b) p = ('a, 'b) func
  let dimap a2b c2d b2c = a2b >> b2c >> c2d
  let first a2b (a, c) = (a2b a), c
  let second b2c (a, b) = a, (b2c b)
  
  
  let left a2b = function
    | Either.Left a -> Either.Left (a2b a)
    | Either.Right r -> Either.Right r

  let right b2c v = match v with
    | Either.Right b -> Either.Right (b2c b)
    | Either.Left l -> Either.Left l
end

(***********************  OPTICS  ********************************)

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

module type CHOICE_OPTIC = sig
  include OPTIC
  module Mk : functor (P : CHOICE) -> sig
    val run : (a, b) P.p -> (s, t) P.p
  end
end

module type STRONGCHOICE_OPTIC = sig
  include OPTIC
  module Mk : functor (P : STRONGCHOICE) -> sig
    val run : (a, b) P.p -> (s, t) P.p
  end
end

let view (type s t a b)
  (lens :
    (module STRONG_OPTIC with type s = s and type t = t and type a = a and type b = b)
    )
  =
  let module L = (val lens) in
  let module R = L.Mk (ForgetF (struct type t = a end)) in
  ofF @@ R.run (Forget (fun x -> x))

let over (type s t a b)
  (lens :
    (module STRONGCHOICE_OPTIC with type s = s and type t = t and type a = a and type b = b)
    )
  (f : a -> b)
  =
  let module L = (val lens) in
  let module R = L.Mk (Func) in
  R.run f

let set l b = over l (fun _ -> b)

let ( ^* ) o l = view l o
let ( *% ) l f = over l f
let ( *= ) l v = set l v

let to' f p = mkF (fun z -> ofF p @@ f z)


type ('s, 't, 'a, 'b) lens = (module STRONG_OPTIC with type s = 's and type t = 't and type a = 'a and type b = 'b)
let mk_lens (lens : (module STRONG_OPTIC with type s = 's and type t = 't and type a = 'a and type b = 'b))
  : ('s, 't, 'a, 'b) lens
  =
  lens


type ('s, 't, 'a, 'b) prism = (module CHOICE_OPTIC with type s = 's and type t = 't and type a = 'a and type b = 'b)
let mk_prism (prism : (module CHOICE_OPTIC with type s = 's and type t = 't and type a = 'a and type b = 'b))
  : ('s, 't, 'a, 'b) prism
  =
  prism


type ('s, 't, 'a, 'b) iso = (module OPTIC with type s = 's and type t = 't and type a = 'a and type b = 'b)
let mk_iso (iso : (module OPTIC with type s = 's and type t = 't and type a = 'a and type b = 'b))
  : ('s, 't, 'a, 'b) iso
  =
  iso

let compose_lens (type ss tt aa bb xx yy) (l1 : (xx, yy, aa, bb) lens) (l2 : (ss, tt, xx, yy) lens)= mk_lens(
  module struct
    type s = ss
    type t = tt
    type a = aa
    type b = bb
    module Mk(P: STRONG) = struct
      let run p =
        let module L1 = (val l1) in
        let module L2 = (val l2) in
        let module R1 = L1.Mk(P) in
        let module R2 = L2.Mk(P) in
        R2.run(R1.run(p))
    end
  end
)