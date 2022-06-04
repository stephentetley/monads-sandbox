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



namespace MonadLib/Monad/State {

    ///
    /// Pure state monad implemented in CPS. 
    ///
    /// A simple implementation of the State monad bursts the stack when `mapAccumLeft` is run on large
    /// collections, so use State the CPS-State monad instead.
    ///
    pub enum State[ka, s, a](s -> (a -> s -> ka) -> ka)

    ///
    /// Helper function for State's implementations of `map` and `ap`.
    ///
    def apply1(ma: State[ka, s, a], st: s, k: a -> s -> ka): ka =
        let State(f) = ma;
        f(st, k)

    ///
    /// Returns the result (new state and answer) of applying `ma` to the initial state `st`.
    ///
    pub def runState(ma: State[(s, a), s, a], st: s): (s, a) =
        let State(f) = ma;
        let cont = (a, s) -> (s, a);
        f(st, cont)


    instance Functor[State[ka, s]] {
        pub def map(f: a -> b & ef, ma: State[ka, s, a]): State[ka, s, b] & ef =
            State((s, k) ->
                apply1(ma, s, (a, s1) -> k(f(a) as & Pure, s1))
            ) as & ef
    }

    instance Applicative[State[ka, s]] {
        pub def point(x: a): State[ka, s, a] =
            State((s, k) -> k(x, s))

        pub def ap(mf: State[ka, s, a -> b & ef], ma: State[ka, s, a]): State[ka, s, b] & ef =
            State((s, k) ->
                apply1(mf, s, (f, s1) ->
                    apply1(ma, s1, (a, s2) ->
                        k(f(a) as & Pure, s2)))
            ) as & ef
    }

    instance Monad[State[ka, s]] {
        pub def flatMap(f: a -> State[ka, s, b] & ef, ma: State[ka, s, a]): State[ka, s, b] & ef =
            flatMap(f, ma)
    }
        
    pub def flatMap(f: a -> State[ka, s, b] & ef, ma: State[ka, s, a]): State[ka, s, b] & ef =
        State((s, k) ->
            apply1(ma, s, (a, s1) ->
                apply1(f(a) as & Pure, s1, (b, s2) ->
                    k(b, s2)))
        ) as & ef

    pub def update(f: s -> s & ef): State[ka, s, Unit] & ef = 
        State((s, k) -> 
            k((), f(s) as & Pure)
        ) as & ef

    pub def put(st: s): State[ka, s, Unit] = 
        State((_, k) -> 
            k((), st)
        )

    pub def get(): State[ka, s, s] = 
        State((s, k) -> 
            k(s, s)
        )

    pub def gets(proj: s -> a & ef): State[ka, s, a] & ef = 
        State((s, k) -> 
            k(proj(s) as & Pure, s)
        ) as & ef

}
