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



namespace MonadLib/ContState {

    ///
    /// ContState is the State monad implemented in CPS. 
    ///
    /// A simple implementation of the State monad bursts the stack when `mapAccumLeft` is run on large
    /// collections, so use ContState the CPS-State monad instead.
    ///
    /// We use an Impure continuation so we can internally cast the `ef` of operations and not lose
    /// impurity information.
    ///
    opaque type ContState[ka, s, a] = (a -> s -> ka & Impure) -> s -> ka & Impure

    ///
    /// Helper function for ContState's implementations of `map` and `ap`.
    ///
    def apply1(ma: ContState[ka, s, a], st: s, k: a -> s -> ka & Impure): ka & Impure =
        let ContState(f) = ma;
        f(k, st)

    ///
    /// Returns the result (new state and answer) of applying `ma` to the initial state `st`.
    ///
    pub def runContState(ma: ContState[(s, a), s, a], st: s): (s, a) & Impure =
        let ContState(f) = ma;
        let cont = (a, s) -> (s, a) as & Impure;
        f(cont, st)


    instance Functor[ContState[ka, s]] {
        pub def map(f: a -> b & ef, ma: ContState[ka, s, a]): ContState[ka, s, b] & ef =
            ContState((k, s) ->
                apply1(ma, s, (a, s1) -> k(f(a) as & Impure, s1))
            ) as & ef
    }

    instance Applicative[ContState[ka, s]] {
        pub def point(x: a): ContState[ka, s, a] =
            ContState((k, s) -> k(x, s))

        pub def ap(mf: ContState[ka, s, a -> b & ef], ma: ContState[ka, s, a]): ContState[ka, s, b] & ef =
            ContState((k, s) ->
                apply1(mf, s, (f, s1) ->
                    apply1(ma, s1, (a, s2) ->
                        k(f(a) as & Impure, s2)))
            ) as & ef
    }

    instance Monad[ContState[ka, s]] {
        pub def flatMap(f: a -> ContState[ka, s, b] & ef, ma: ContState[ka, s, a]): ContState[ka, s, b] & ef =
            ContState((k, s) ->
                apply1(ma, s, (a, s1) ->
                    apply1(f(a) as & Impure, s1, (b, s2) ->
                        k(b, s2)))
            ) as & ef
    }

    /// update is tricky, we must update the state for the rest of the computation, are we sure this works...

    pub def update(f: s -> s & ef): ContState[ka, s, Unit] & ef = 
        ContState((k, s) -> 
            k((), f(s) as & Impure)
        ) as & ef

    pub def put(st: s): ContState[ka, s, Unit] = 
        ContState((k, _) -> 
            k((), st)
        )

    pub def get(): ContState[ka, s, s] = 
        ContState((k, s) -> 
            k(s, s)
        )

    pub def gets(proj: s -> a & ef): ContState[ka, s, a] & ef = 
        ContState((k, s) -> 
            k(proj(s) as & Impure, s)
        ) as & ef

}
