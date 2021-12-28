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



namespace MonadLib/ContReader {

    ///
    /// ContReader is the Reader monad implemented in CPS. 
    ///
    /// We use an Impure continuation so we can internally cast the `ef` of operations and not lose
    /// impurity information.
    ///
    opaque type ContReader[ka, r, a] = r -> (a -> ka & Impure) -> ka & Impure

    ///
    /// Helper function for ContReader's implementations of `map` and `ap`.
    ///
    def apply1(ma: ContReader[ka, r, a], r: r, k: a -> ka & Impure): ka & Impure =
        let ContReader(f) = ma;
        f(r, k)


    ///
    /// Returns the result of applying `ma` to `r`.
    ///
    pub def runContReader(ma: ContReader[a, r, a], r: r): a & Impure =
        let ContReader(f) = ma;
        f(r, k -> k as & Impure)


    instance Functor[ContReader[ka, r]] {
        pub def map(f: a -> b & ef, ma: ContReader[ka, r, a]): ContReader[ka, r, b] & ef =
            ContReader((r, k) ->
                apply1(ma, r, v1 -> k(f(v1) as & Impure))
            ) as & ef
    }

    instance Applicative[ContReader[ka, r]] {
        pub def point(x: a): ContReader[ka, r, a] =
            ContReader((_, k) -> k(x))

        pub def ap(mf: ContReader[ka, r, a -> b & ef], ma: ContReader[ka, r, a]): ContReader[ka, r, b] & ef =
            ContReader((r, k) ->
                apply1(mf, r, f1 ->
                    apply1(ma, r, a1 ->
                        k(f1(a1) as & Impure)))
            ) as & ef
    }

    instance Monad[ContReader[ka, r]] {
        pub def flatMap(f: a -> ContReader[ka, r, b] & ef, ma: ContReader[ka, r, a]): ContReader[ka, r, b] & ef =
            ContReader((r, k) ->
                apply1(ma, r, a1 ->
                    apply1(f(a1) as & Impure, r, b1 ->
                        k(b1)))
            ) as & ef
    }

    pub def ask(): ContReader[ka, r, r] = 
        ContReader((r, k) -> 
            k(r)
        )

    pub def asks(f: s -> a & ef): ContReader[ka, s, a] & ef = 
        ContReader((r, k) -> 
            k(f(r) as & Impure)
        ) as & ef


/*
    /// update is tricky, we must update the state for the rest of the computation, are we sure this works...


    pub def put(st: s): ContReader[ka, s, Unit] = 
        ContReader((k, _) -> 
            k((), st)
        )


    pub def gets(proj: s -> a & ef): ContReader[ka, s, a] & ef = 
        ContReader((k, s) -> 
            k(proj(s) as & Impure, s)
        ) as & ef

*/

}
