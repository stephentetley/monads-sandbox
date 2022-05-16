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


namespace MonadLib/Simple/State {

    ///
    /// State monad should only be used with immutable state types!
    ///
    /// Use MutState monad if you have mutable state
    ///
    pub opaque type State[st: Type, a: Type] = st -> (st, a)

    


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: State[st, a], s: st): (st, a) =
        let State(ma1) = ma;
        ma1(s)



    ///
    /// Runs `ma` returning an `Option`.
    ///
    pub def runState(ma: State[st, a], s: st): (st, a) =
        apply1(ma, s)


    instance Functor[State[st]] {
        pub def map(f: a -> b & ef, ma: State[st, a]): State[st, b] & ef =
            State(s ->
                let (s1, a) = apply1(ma, s);
                (s1, f(a) as & Pure)
            ) as & ef
    }


    instance Applicative[State[st]] {
        pub def point(x: a): State[st, a] = State(s -> (s, x))

        pub def ap(mf: State[st, a -> b & ef], ma: State[st, a]): State[st, b] & ef =
            State(s ->
                let (s1, f) = apply1(mf, s);
                let (s2, a) = apply1(ma, s1);
                (s2, f(a) as & Pure)
            ) as & ef
    }


    instance Monad[State[st]] {
        pub def flatMap(f: a -> State[st, b] & ef, ma: State[st, a]): State[st, b] & ef =
            flatMap(f, ma)
    }

    pub def flatMap(f: a -> State[st, b] & ef, ma: State[st, a]): State[st, b] & ef =
        State(s ->
            let (s1, a) = apply1(ma, s);
            apply1(f(a) as & Pure, s1)
        ) as & ef
  
    pub def getState(): State[st, st] =
        State(s -> (s, s))

    pub def setState(st: st): State[st, Unit] =
        State(_ -> (st, ()))

    pub def sets(upd: st -> st & ef): State[st, Unit] & ef =
        State(s -> (upd(s) as & Pure, ())) as & ef

}
