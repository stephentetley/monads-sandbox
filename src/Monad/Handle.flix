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



/// Handle is a monad for Impure state
/// We don't need to pair the state/handle in the return type (c.f. the pure state monad)
/// as it would just be an alias of the handle on the LHS.
/// Basically the monad is Reader but local is not allowed as we cannot reset the state.

/// For real world use this should written in CPS...

namespace MonadLib/Monad/Handle {

    ///
    /// Handle monad with encapsulated effect
    ///
    pub opaque type Handle[ef: Bool, h: Type, a: Type] = h -> a & ef

 


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    pub def apply1(ma: Handle[ef, h, a], h: h): a & ef =
        let Handle(ma1) = ma;
        ma1(h)


    ///
    /// Runs `ma` on a handle
    ///
    pub def runHandle(ma: Handle[ef, h, a], h: h): a & ef =
        apply1(ma, h)
        
     
    /// Ideally we want stronger contraints on `update` and `asks` 
    /// `update` should only be allowed on "writeable" handle
    /// `asks` should only be allowed on a "readable" handle


    pub def update(upd: h -> Unit & ef): Handle[ef, h, Unit] =
        Handle(h -> upd(h))

    pub def asks(f: h -> a & ef): Handle[ef, h, a] =
        Handle(h -> f(h))


    instance Functor[Handle[ef, h]] {
        pub def map(f: a -> b & ef1, ma: Handle[ef, h, a]): Handle[ef, h, b] & ef1 =
            Handle(h ->
                let a = apply1(ma, h);
                f(a) as & ef
            ) as & ef1
    }

    instance Applicative[Handle[ef, h]] {
        pub def point(x: a): Handle[ef, h, a] = Handle(_ -> x as & ef)

        pub def ap(mf: Handle[ef, h, a -> b & ef1], ma: Handle[ef, h, a]): Handle[ef, h, b] & ef1 =
            Handle(h ->
                let f = apply1(mf, h);
                let a = apply1(ma, h);
                f(a) as & ef
            ) as & ef1
    }


    instance Monad[Handle[ef, h]] {
        pub def flatMap(f: a -> Handle[ef, h, b] & ef1, ma: Handle[ef, h, a]): Handle[ef, h, b] & ef1 =
            flatMap(f, ma)
    }

    pub def flatMap(f: a -> Handle[ef, h, b] & ef1, ma: Handle[ef, h, a]): Handle[ef, h, b] & ef1 =
        Handle(h ->
            let a = apply1(ma, h);
            apply1(f(a) as & ef, h)
        ) as & ef1

/*
    pub def getState(): State[ef, st, st] =
        State(s -> (s, s) as & ef)

    pub def setState(st: st): State[ef, st, Unit] =
        State(_ -> (st, ()) as & ef)

*/

}
