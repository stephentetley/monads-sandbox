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



/// Codensity monad for mutable state
/// We don't need to pair the state/handle in the return type (c.f. the pure state monad)
/// as it would just be an alias of the handle on the LHS.
/// Basically the monad is ContReader but local is not allowed as we cannot reset the state.


namespace MonadLib/Monad/MutState {

    ///
    /// MutState monad with encapsulated effect
    ///
    pub opaque type MutState[ef: Bool, ka: Type, st: Type, a: Type] = st -> (a -> ka & ef) -> ka & ef



    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    pub def apply1(ma: MutState[ef, ka, st, a], s: st, k: a -> ka & ef): ka & ef =
        let MutState(f) = ma;
        f(s, k)


    ///
    /// Runs `ma` on a handle
    ///
    pub def runHandle(ma: MutState[ef, a, st, a], s: st): a & ef =
        apply1(ma, s, k -> k as & ef)
        
   
    /// Ideally we want stronger contraints on `update` and `asks` 
    /// `update` should only be allowed on "writeable" handle
    /// `asks` should only be allowed on a "readable" handle


    pub def update(upd: st -> Unit & ef): MutState[ef, ka, st, Unit] =
        MutState((s, k) -> {upd(s); k()})
  
    pub def asks(f: st -> a & ef): MutState[ef, ka, st, a] =
        MutState((s, k) -> {let ans = f(s); k(ans)})


    instance Functor[MutState[ef, ka, st]] {
        pub def map(f: a -> b & ef1, ma: MutState[ef, ka, st, a]): MutState[ef, ka, st, b] & ef1 =
            MutState((s, k) ->
                apply1(ma, s, v1 -> k(f(v1) as & ef))
            ) as & ef1
    }


    instance Applicative[MutState[ef, ka, st]] {
        pub def point(x: a): MutState[ef, ka, st, a] = 
            MutState((_, k) -> k(x))

        pub def ap(mf: MutState[ef, ka, st, a -> b & ef1], ma: MutState[ef, ka, st, a]): MutState[ef, ka, st, b] & ef1 =
            MutState((s, k) ->
                apply1(mf, s, f1 ->
                    apply1(ma, s, a1 ->
                        k(f1(a1) as & ef)))
            ) as & ef1
    }


    instance Monad[MutState[ef, ka, st]] {
        pub def flatMap(f: a -> MutState[ef, ka, st, b] & ef1, ma: MutState[ef, ka, st, a]): MutState[ef, ka, st, b] & ef1 =
            flatMap(f, ma)
    }


    pub def flatMap(f: a -> MutState[ef, ka, st, b] & ef1, ma: MutState[ef, ka, st, a]): MutState[ef, ka, st, b] & ef1 =
        MutState((s, k) ->
            apply1(ma, s, v1 ->
                apply1(f(v1) as & ef, s, v2 ->
                    k(v2)))
        ) as & ef1


}
