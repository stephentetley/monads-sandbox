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

namespace MonadLib/Monad/Writer {

    ///
    /// Pure Writer monad implemented in CPS. 
    ///
    /// The writer type should be pure, if you want to use StringBuilder or another mutable type use MutState.
    ///
    pub enum Writer[ka, w, a]((a -> w -> ka) -> ka)


    instance Functor[Writer[ka, w]] {
        pub def map(f: a -> b & ef, ma: Writer[ka, w, a]): Writer[ka, w, b] & ef =
            Writer(k ->
                apply1(ma, (a1, w1) -> k(f(a1) as & Pure, w1))
            ) as & ef
    }


    instance Applicative[Writer[ka, w]] with Monoid[w] {
        pub def point(x: a): Writer[ka, w, a] =
            Writer(k -> k(x, Monoid.empty()))

        pub def ap(mf: Writer[ka, w, a -> b & ef], ma: Writer[ka, w, a]): Writer[ka, w, b] & ef =
            Writer(k ->
                apply1(mf, (f1, w1) ->
                    apply1(ma, (a1, w2) ->
                        k(f1(a1) as & Pure, Monoid.combine(w1, w2))))
            ) as & ef
    }


    instance Monad[Writer[ka, w]] with Monoid[w] {
        pub def flatMap(f: a -> Writer[ka, w, b] & ef, ma: Writer[ka, w, a]): Writer[ka, w, b] & ef =
            flatMap(f, ma)
    }

    ///
    /// Helper function for Writer's implementations of `map` and `ap`.
    ///
    def apply1(ma: Writer[ka, w, a], k: a -> w -> ka): ka =
        let Writer(f) = ma;
        f(k)


    ///
    /// Returns the result of `ma`.
    ///
    pub def runWriter(ma: Writer[(a, w), w, a]): (a, w) =
        let Writer(f) = ma;
        let cont = (a, w) -> (a, w);
        f(cont)

    ///
    /// Returns just the "output" of `ma`.
    ///
    pub def execWriter(ma: Writer[w, w, a]): w =
        let Writer(f) = ma;
        let cont = (_, w) -> w;
        f(cont)

    
    pub def flatMap(f: a -> Writer[ka, w, b] & ef, ma: Writer[ka, w, a]): Writer[ka, w, b] & ef with Monoid[w] =
        Writer(k ->
            apply1(ma, (a1, w1) ->
                apply1(f(a1) as & Pure, (b1, w2) ->
                    k(b1, Monoid.combine(w1, w2))))
        ) as & ef


    pub def tell(a: w): Writer[ka, w, Unit] = 
        Writer(k -> 
            k((), a)
        )


}
