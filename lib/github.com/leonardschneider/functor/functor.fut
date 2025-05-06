
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

  type^ F 't 'u 'v 'a 'b 'c [s] = {
    map: (f: t -> u) -> a -> b,
    map2: (f: t -> u -> v) -> a -> b -> c,
    reduce: (f: t -> t -> t) -> t -> a -> t,
    flatten: a -> [s]t,
    unflatten: [s]t -> a
  }

  let map 't 'u 'v 'a 'b 'c [s] (f: F t u v a b c [s]) = f.map

  let map2 't 'u 'v 'a 'b 'c [s] (f: F t u v a b c [s]) = f.map2

  let length 't 'u 'v 'a 'b 'c [s] (_: F t u v a b c [s]) = s

  let reduce 't 'u 'v 'a 'b 'c [s] (f: F t u v a b c [s]) = f.reduce

  let flatten 't 'u 'v 'a 'b 'c [s] (f: F t u v a b c [s]) = f.flatten

  let unflatten 't 'u 'v 'a 'b 'c [s] (f: F t u v a b c [s]) = f.unflatten

  let tabulate 't 'u 'v 'a 'b 'c [s] (p: F t u v a b c [s]) = \f ->
    pre.tabulate s f |> p.unflatten


  -- | Scalar mapping
  let scalar 't 'u 'v: F t u v t u v [1] = {
    map = \f -> f,
    map2 = \f -> f,
    reduce = \_ -> \_ -> \x -> x,
    flatten = \x -> [x],
    unflatten = \x -> x[0]
  }

  -- | Array mapping
  let array 't 'u 'v 'a 'b 'c [s] (n: i64)
    (p: F t u v a b c [s]): F t u v ([n]a) ([n]b) ([n]c) [n*s] = {
    map = \f -> \x -> pre.map (p.map f) x,
    map2 = \f -> \x y -> pre.map2 (p.map2 f) x y,
    reduce = \f -> \ne -> \as -> pre.map (p.reduce f ne) as |> pre.reduce f ne,
    flatten = \x -> pre.map p.flatten x |> pre.flatten,
    unflatten = \x -> pre.unflatten x |> pre.map p.unflatten,
  }

  -- | Tuple mapping
  let pair 't 'u 'v 'a0 'b0 'c0 'a1 'b1 'c1 [s0] [s1]
    (p0: F t u v a0 b0 c0 [s0])
    (p1: F t u v a1 b1 c1 [s1]): F t u v (a0, a1) (b0, b1) (c0, c1) [s0+s1] = {
    map = \f -> \x -> (p0.map f x.0, p1.map f x.1),
    map2 = \f -> \x y -> (p0.map2 f x.0 y.0, p1.map2 f x.1 y.1),
    reduce = \f -> \ne -> \x -> f (p0.reduce f ne x.0) (p1.reduce f ne x.1),
    flatten = \x -> p0.flatten x.0 ++ p1.flatten x.1,
    unflatten = \x -> (p0.unflatten (take s0 x), p1.unflatten ((drop s0 x) :> [s1]t)),
  }

}



