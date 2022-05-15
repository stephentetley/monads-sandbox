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

/// Note - I have realized this is wrong.
/// If we have mutable state all we need is a Handle monad - pairing the state in the answer
/// just leads to aliasing.

namespace MonadLib/Monad/SimpleState {

    ///
    /// State monad with polymorphic effect
    ///
    /// Note - if we have impure state we can't allow backtracking.
    /// (Backtracking is not in the StateMonad API but we must be careful not to use this as a template
    /// for more "full featured" monads)
    ///
    pub opaque type State[ef: Bool, st: Type, a: Type] = st -> (st, a) & ef

    


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: State[ef, st, a], s: st): (st, a) & ef =
        let State(ma1) = ma;
        ma1(s)



    ///
    /// Runs `ma` returning an `Option`.
    ///
    pub def runState(ma: State[ef, st, a], s: st): (st, a) & ef =
        apply1(ma, s)


    instance Functor[State[ef, st]] {
        pub def map(f: a -> b & ef1, ma: State[ef, st, a]): State[ef, st, b] & ef1 =
            State(s ->
                let (s1, a) = apply1(ma, s);
                (s1, f(a) as & ef)
            ) as & ef1
    }


    instance Applicative[State[ef, st]] {
        pub def point(x: a): State[ef, st, a] = State(s -> (s, x) as & ef)

        pub def ap(mf: State[ef, st, a -> b & ef1], ma: State[ef, st, a]): State[ef, st, b] & ef1 =
            State(s ->
                let (s1, f) = apply1(mf, s);
                let (s2, a) = apply1(ma, s1);
                (s2, f(a) as & ef)
            ) as & ef1
    }


    instance Monad[State[ef, st]] {
        pub def flatMap(f: a -> State[ef, st, b] & ef1, ma: State[ef, st, a]): State[ef, st, b] & ef1 =
            flatMap(f, ma)
    }

    pub def flatMap(f: a -> State[ef, st, b] & ef1, ma: State[ef, st, a]): State[ef, st, b] & ef1 =
        State(s ->
            let (s1, a) = apply1(ma, s);
            apply1(f(a) as & ef, s1)
        ) as & ef1
  
    pub def getState(): State[ef, st, st] =
        State(s -> (s, s) as & ef)

    pub def setState(st: st): State[ef, st, Unit] =
        State(_ -> (st, ()) as & ef)

    pub def sets(upd: st -> st & ef1): State[ef1, st, Unit] =
        State(s -> (upd(s), ()))

}
