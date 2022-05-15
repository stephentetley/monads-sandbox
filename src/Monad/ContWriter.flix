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

namespace MonadLib/Monad/ContWriter {

    ///
    /// ContWriter is the Writer monad implemented in CPS. 
    ///
    /// We use an Impure continuation so we can internally cast the `ef` of operations and not lose
    /// impurity information.
    ///
    pub opaque type ContWriter[ka, w, a] = (a -> w -> ka & Impure) -> ka & Impure


    instance Functor[ContWriter[ka, w]] {
        pub def map(f: a -> b & ef, ma: ContWriter[ka, w, a]): ContWriter[ka, w, b] & ef =
            map(f, ma)
    }


    instance Applicative[ContWriter[ka, w]] with Monoid[w] {
        pub def point(x: a): ContWriter[ka, w, a] =
            point(x)

        pub def ap(mf: ContWriter[ka, w, a -> b & ef], ma: ContWriter[ka, w, a]): ContWriter[ka, w, b] & ef =
            ap(mf, ma)
    }


    instance Monad[ContWriter[ka, w]] with Monoid[w] {
        pub def flatMap(f: a -> ContWriter[ka, w, b] & ef, ma: ContWriter[ka, w, a]): ContWriter[ka, w, b] & ef =
            flatMap(f, ma)
    }

    ///
    /// Helper function for ContWriter's implementations of `map` and `ap`.
    ///
    def apply1(ma: ContWriter[ka, w, a], k: a -> w -> ka & Impure): ka & Impure =
        let ContWriter(f) = ma;
        f(k)


    ///
    /// Returns the result of `ma`.
    ///
    pub def runContWriter(ma: ContWriter[(a, w), w, a]): (a, w) & Impure =
        let ContWriter(f) = ma;
        let cont = (a, w) -> (a, w) as & Impure;
        f(cont)

    ///
    /// Returns just the "output" of `ma`.
    ///
    pub def execContWriter(ma: ContWriter[w, w, a]): w & Impure =
        let ContWriter(f) = ma;
        let cont = (_, w) -> w as & Impure;
        f(cont)

    
    pub def map(f: a -> b & ef, ma: ContWriter[ka, w, a]): ContWriter[ka, w, b] & ef =
            ContWriter(k ->
                apply1(ma, (a1, w1) -> k(f(a1) as & Impure, w1))
            ) as & ef

    pub def point(x: a): ContWriter[ka, w, a] with Monoid[w] =
            ContWriter(k -> k(x, Monoid.empty()))


    pub def ap(mf: ContWriter[ka, w, a -> b & ef], ma: ContWriter[ka, w, a]): ContWriter[ka, w, b] & ef with Monoid[w] =
        ContWriter(k ->
            apply1(mf, (f1, w1) ->
                apply1(ma, (a1, w2) ->
                    k(f1(a1) as & Impure, Monoid.combine(w1, w2))))
        ) as & ef

    pub def flatMap(f: a -> ContWriter[ka, w, b] & ef, ma: ContWriter[ka, w, a]): ContWriter[ka, w, b] & ef with Monoid[w] =
        ContWriter(k ->
            apply1(ma, (a1, w1) ->
                apply1(f(a1) as & Impure, (b1, w2) ->
                    k(b1, Monoid.combine(w1, w2))))
        ) as & ef


    pub def tell(a: w): ContWriter[ka, w, Unit] = 
        ContWriter(k -> 
            k(() as & Impure, a)
        )


}
