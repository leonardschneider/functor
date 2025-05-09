
-- | Homegeneous functor
-- | A functor is a type constructor that can be mapped over.

module pre = {
  let map = map
  let map2 = map2
  let reduce = reduce
  let flatten = flatten
  let unflatten = unflatten
  let tabulate = tabulate
}

module F = {

  type^ F 't 'a [s] = {
    flatten: a -> [s]t,
    unflatten: [s]t -> a
  }

  let cst 't 'a (a0: a): F t a [0] = {
    flatten = \_ -> [],
    unflatten = \_ -> a0
  }

  let bimap 't 'a 'b [s] (f: a -> b) (g: b -> a) (p: F t a [s]): F t b [s] = {
    flatten = \x -> x |> g |> p.flatten,
    unflatten = \x -> x |> p.unflatten |> f
  }

  let length 't 'a [s] (_: F t a [s]) = s

  let flatten 't 'a [s] (f: F t a [s]) = f.flatten

  let unflatten 't 'a [s] (f: F t a [s]) = f.unflatten


  -- | Scalar mapping
  let scalar 't: F t t [1] = {
    flatten = \x -> [x],
    unflatten = \x -> x[0]
  }

  -- | Array mapping
  let array 't 'a [s] (n: i64)
    (p: F t a [s]): F t ([n]a) [n*s] = {
    flatten = \x -> pre.map p.flatten x |> pre.flatten,
    unflatten = \x -> pre.unflatten x |> pre.map p.unflatten,
  }

  -- | Tuple mapping
  let pair 't 'a 'b [s0] [s1]
    (p0: F t a [s0])
    (p1: F t b [s1]): F t (a, b) [s0+s1] = {
    flatten = \x -> p0.flatten x.0 ++ p1.flatten x.1,
    unflatten = \x -> (p0.unflatten (take s0 x), p1.unflatten ((drop s0 x) :> [s1]t)),
  }

}



