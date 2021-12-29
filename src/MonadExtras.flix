/*
 * Copyright 2021 Stephen Tetley
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



/// Result has "wrong" arg order.

namespace MonadExtras {
    
    use Applicative.{point};
    use Alternative.{alt, zero};
    use Monad.{flatMap};

    pub def bind(x: m[a], k: a -> m[b] & ef): m[b] & ef with Monad[m] = Monad.flatMap(k, x)



    pub def ifM(test: m[Bool], mthen: m[a], melse: m[a]): m[a] with Monad[m] =
        flatMap(x1 -> if (x1) mthen else melse, test)
    
    pub def whenTrueM(x: m[Bool], f: m[Unit]): m[Unit] with Monad[m] =
        flatMap(x1 -> if (x1) f else point(()), x)


    pub def whenFalseM(x: m[Bool], f: m[Unit]): m[Unit] with Monad[m] =
        flatMap(x1 -> if (not x1) f else point(()), x)


    pub def mfilter(f: a -> Bool, mx: m[a]): m[a] with Alternative[m], Monad[m] = 
        flatMap(x -> if (f(x)) point(x) else zero(), mx)

    pub def foldLeftM(f: (b, a) -> m[b], s: b, xs: List[a]): m[b] with Monad[m] = 
        foldLeftMHelper(f, point(s), xs)

    def foldLeftMHelper(f: (b, a) -> m[b], s: m[b], xs: List[a]): m[b] with Monad[m] = match xs {
        case Nil => s
        case x :: rs => foldLeftMHelper(f, bind(s, s1 -> f(s1, x)), rs)
    }

    pub def foldRightM(f: (a, b) -> m[b], s: b, xs: List[a]): m[b] with Monad[m] = 
        foldRightMHelper(f, point(s), xs, ms -> ms)

    ///
    /// Helper function for `foldRightM`.
    ///
    def foldRightMHelper(f: (a, b) -> m[b], s: m[b], xs: List[a], k: m[b] -> m[b]): m[b] with Monad[m] = match xs {
        case Nil => k(s)
        case x :: rs => foldRightMHelper(f, s, rs, ms -> k(bind(ms, s1 -> f(x, s1))))
    }


    /// Chains are too "special case" for adding to the Monad namespace.
    /// But they are complicated to define and more general than being tied
    /// to a specific parser combinator library...
    /// Where should they go?
    /// [Also note - chains might be a headache for a strict language...]
    pub def chainLeft(p: m[a], op: m[(a, a) -> a], x: a): m[a] with Alternative[m], Monad[m] =
        Alternative.alt(chainLeft1(p,op), Applicative.point(x))

    pub def chainLeft1(p: m[a], op: m[(a, a) -> a]): m[a] with Alternative[m], Monad[m] =
        flatMap(v1 -> chainLeft1Helper(p, op, v1), p)

    def chainLeft1Helper(p: m[a], op: m[(a, a) -> a], x: a): m[a] with Alternative[m], Monad[m] =
        alt(flatMap(f -> flatMap(v1 -> chainLeft1Helper(p, op, f(x, v1)), p), op)
           , point(x))



    pub def chainRight(p: m[a], op: m[(a, a) -> a], x: a): m[a] with Alternative[m], Monad[m] =
        alt(chainRight1(p, op), point(x))


    pub def chainRight1(p: m[a], op: m[(a, a) -> a]): m[a] with Alternative[m], Monad[m] =
        flatMap(v1 -> chainRight1Helper(p, op, v1), p)

    def chainRight1Helper(p: m[a], op: m[(a, a) -> a], x: a): m[a] with Alternative[m], Monad[m] =
        alt(chainRight1Step(p, op, x), point(x))

    def chainRight1Step(p: m[a], op: m[(a, a) -> a], x: a): m[a] with Alternative[m], Monad[m] =
        flatMap(f ->
            flatMap(y -> flatMap(point, chainRight1Helper(p,op, f(x,y))), flatMap(v1 -> chainRight1Helper(p, op, v1), p)), op)

}