/*
 * Copyright 2022 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/// This module is an experiment - ideally `traverseM` would be a method of the `Traversable`
/// type class 

pub class TraversableM[t: Type -> Type] with Traversable[t] {

    ///
    /// The Monad implementation of `flatMap` must be stack-safe for instances `traverseM` to be stack-safe.
    /// If the Monad supports "short-circuiting" on failure in its implementation of `flatMap` then `traverseM`
    /// will short-circuit on the container traversal on failure.
    ///
    pub def traverseM(f: a -> m[b] \ ef, t: t[a]): m[t[b]] \ ef with Monad[m]

}

instance TraversableM[List] {
    pub def traverseM(f: a -> m[b] \ ef, l: List[a]): m[List[b]] \ ef with Monad[m] = 
        TraversableMHelper.traverseM_List(f, l)
}

instance TraversableM[Chain] {
    pub def traverseM(f: a -> m[b] \ ef, c: Chain[a]): m[Chain[b]] \ ef with Monad[m] = 
        TraversableMHelper.traverseM_chain(f, c)
}


namespace TraversableMHelper {


    use Monad.{>>=};

    /// The implementation of `flatMap` for the Monad instance must be stack safe.
    /// If the Monad instance supports failure the traversal short circuits on the first failure.
    pub def traverseM_List(f: a -> m[b] \ ef, l: List[a]): m[List[b]] \ ef with Monad[m] = 
        def loop(ll, k) = match ll {
            case Nil => k(Nil)
            case x :: xs => f(x) >>= (a1 -> loop(xs, ks -> k(a1 :: ks)))
        };
        loop(l, upcast(Applicative.point))

    pub def traverseM_chain(f: a -> m[b] \ ef, c: Chain[a]): m[Chain[b]] \ ef with Monad[m] =
        use Chain.ViewLeft.{SomeLeft, NoneLeft};
        def loop(v, acc) = match v {
            case NoneLeft        => Applicative.point(acc)
            case SomeLeft(x, rs) => f(x) >>= (a1 -> loop(Chain.viewLeft(rs), Chain.snoc(acc, a1)))
        };
        loop(Chain.viewLeft(c), Chain.empty())
}


