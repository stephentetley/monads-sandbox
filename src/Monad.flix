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


/// This is a temporary module until the stdlib gets a Monad class.



/// Result has "wrong" arg order.

namespace MonadTemp {
    
    use Applicative.{point};
    use Alternative.{alt, zero};
    use Monad.{flatMap};

    pub def bind(x: m[a], k: a -> m[b] & ef): m[b] & ef with Monad[m] = Monad.flatMap(k, x)


    pub def >>=(x: m[a], k: a -> m[b] & ef): m[b] & ef with Monad[m] =
        bind(x, k)

    pub def =<<(k: a -> m[b] & ef, x: m[a]): m[b] & ef with Monad[m] =
        bind(x, k)


    pub def kleisliLeft(f1: a -> m[b] & ef, f2: b -> m[c] & ef): a -> m[c] & ef with Monad[m] = x ->
        bind(f1(x), x1 -> f2(x1))


    pub def kleisliRight(f1: b -> m[c] & ef, f2: a -> m[b] & ef): a -> m[c] & ef with Monad[m] = x -> 
        bind(f2(x), x1 -> f1(x1))      
    
    /// Alias for `kleisliLeft`
    pub def >=>(f1: a -> m[b] & ef, f2: b -> m[c] & ef): a -> m[c]  & ef with Monad[m] = x ->
        kleisliLeft(f1, f2, x)

    /// Alias for `kleisliRight`
    pub def <=<(f1: b -> m[c] & ef, f2: a -> m[b] & ef): a -> m[c] & ef with Monad[m] = x -> 
        kleisliRight(f1, f2, x)

    pub def whenTrueM(test: m[Bool], ma: m[a]): m[Option[a]] with Monad[m] = 
        bind(test, x -> ApplicativeTemp.whenTrue(x, ma))

    pub def whenFalseM(test: m[Bool], ma: m[a]): m[Option[a]] with Monad[m] = 
        bind(test, x -> ApplicativeTemp.whenFalse(x, ma))

    pub def mfilter(f: a -> Bool, mx: m[a]): m[a] with Alternative[m], Monad[m] = 
        bind(mx, x -> if (f(x)) point(x) else zero())

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


/*
    /// Chains

    pub def chainl[a, m :# Type -> Type : Monad : Alternative](p: m[a], op: m[(a, a) -> a], x: a): m[a] = 
        alt(chainl1(p,op), point(x))

    pub def chainl1[a, m :# Type -> Type : Monad : Alternative](p: m[a], op: m[(a, a) -> a]): m[a] = 
        bind(p, v1 -> chainl1Helper(p, op, v1))
        
    def chainl1Helper[a, m :# Type -> Type : Monad : Alternative](p: m[a], op: m[(a, a) -> a], x: a): m[a] = 
        alt(bind(op, f -> bind(p, v1 -> chainl1Helper(p, op, f(x, v1))))
            , point(x))


    pub def chainr1[a, m :# Type -> Type : Monad : Alternative](p: m[a], op: m[(a, a) -> a]): m[a] = 
        bind(p, v1 -> chainr1Helper(p, op, v1))
    
    def chainr1Helper[a, m :# Type -> Type : Monad : Alternative](p: m[a], op: m[(a, a) -> a], x: a): m[a] = 
        alt(chainr1Step(p, op, x), point(x))

    def chainr1Step[a, m :# Type -> Type : Monad : Alternative](p: m[a], op: m[(a, a) ->a], x: a): m[a] = 
        bind(op, f -> 
            bind(bind(p, v1 -> chainr1Helper(p, op, v1)), y -> 
                bind(chainr1Helper(p,op, f(x,y)), ans -> point(ans))))

*/

}