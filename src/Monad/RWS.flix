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



namespace MonadLib/Monad/RWS {



    ///
    /// Pure Reader-Writer-State monad implemented in CPS. 
    ///
    /// We use an Impure continuation so we can internally cast the `ef` of operations and not lose
    /// impurity information.
    ///
    pub enum RWS[ka, r, w, s, a](r -> s -> w -> (a -> s -> w -> ka) -> ka)



    ///
    /// Helper function for RWS's implementations of `map` and `ap`.
    ///
    def apply1(ma: RWS[ka, r, w, s, a], r: r, s: s, w: w, k: a -> s -> w -> ka): ka =
        let RWS(f) = ma;
        f(r, s, w, k)


    ///
    /// Returns the result of `ma`.
    ///
    pub def runRWS(ma: RWS[(a, s, w), r, w, s, a], r: r, s: s): (a, s, w) with Monoid[w] =
        let RWS(f) = ma;
        let cont = (a, s, w) -> (a, s, w);
        f(r, s, Monoid.empty(), cont)

    ///
    /// Returns answer and output `ma`.
    ///
    pub def evalRWS(ma: RWS[(a, s, w), r, w, s, a], r: r, s: s): (a, w) with Monoid[w] =
        let (a, _, w) = runRWS(ma, r, s);
        (a, w)


    ///
    /// Returns final state and output `ma`.
    ///
    pub def execRWS(ma: RWS[(a, s, w), r, w, s, a], r: r, s: s): (s, w) with Monoid[w] =
        let (_, s1, w) = runRWS(ma, r, s);
        (s1, w)


    instance Functor[RWS[ka, r, w, s]] {
        pub def map(f: a -> b & ef, ma: RWS[ka, r, w, s, a]): RWS[ka, r, w, s, b] & ef =
            RWS((r, s, w, k) ->
                apply1(ma, r, s, w, (a1, s1, w1) -> k(f(a1) as & Pure, s1, w1))
            ) as & ef
    }


    instance Applicative[RWS[ka, r, w, s]] {
        pub def point(x: a): RWS[ka, r, w, s, a] =
            RWS((_, s, w, k) -> k(x, s, w))

        pub def ap(mf: RWS[ka, r, w, s, a -> b & ef], ma: RWS[ka, r, w, s, a]): RWS[ka, r, w, s, b] & ef =
            RWS((r, s, w, k) ->
                apply1(mf, r, s, w, (f1, s1, w1) ->
                    apply1(ma, r, s1, w1, (a1, s2, w2) ->
                        k(f1(a1) as & Pure, s2, w2)))
            ) as & ef
    }


    instance Monad[RWS[ka, r, w, s]] {
        pub def flatMap(f: a -> RWS[ka, r, w, s, b] & ef, ma: RWS[ka, r, w, s, a]): RWS[ka, r, w, s, b] & ef =
            flatMap(f, ma)
    }

    pub def flatMap(f: a -> RWS[ka, r, w, s, b] & ef, ma: RWS[ka, r, w, s, a]): RWS[ka, r, w, s, b] & ef =
        RWS((r, s, w, k) ->
            apply1(ma, r, s, w, (a1, s1, w1) ->
                apply1(f(a1) as & Pure, r, s1, w1, (b1, s2, w2) ->
                    k(b1, s2, w2)))
        ) as & ef

    pub def tell(a: w): RWS[ka, r, w, s, Unit] with SemiGroup[w] = 
        RWS((_, s, w, k) ->
            k((), s, w ++ a)
        )


    pub def ask(): RWS[ka, r, w, s, r] = 
        RWS((r, s, w, k) ->
            k(r, s, w)
        )

    pub def asks(f: r -> a & ef): RWS[ka, r, w, s, a] & ef =
        RWS((r, s, w, k) ->
            k(f(r) as & Pure, s, w)
        ) as & ef

    pub def local(f: r -> r, ma: RWS[ka, r, w, s, a]): RWS[ka, r, w, s, a] & ef = 
        RWS((r, s, w, k) ->
            apply1(ma, f(r), s, w, a1 -> 
                k(a1))
        ) as & ef

    pub def update(f: s -> s & ef): RWS[ka, r, w, s, Unit] & ef = 
        RWS((_, s, w, k) ->
            k((), f(s) as & Pure, w)
        ) as & ef

    pub def put(st: s): RWS[ka, r, w, s, Unit]  = 
        RWS((_, _, w, k) ->
            k((), st, w)
        )

    pub def get(): RWS[ka, r, w, s, s] =  
        RWS((_, s, w, k) ->
            k(s, s, w)
        )

    pub def gets(proj: s -> a & ef): RWS[ka, r, w, s, a] & ef =  
        RWS((_, s, w, k) ->
            k(proj(s) as & Pure, s, w)
        ) as & ef

}
