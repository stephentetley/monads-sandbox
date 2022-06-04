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



namespace MonadLib/Monad/Maybe {

    ///
    /// Maybe is the codensity version of the Maybe monad.
    ///
    pub enum Maybe[ka, a]((a -> Option[ka]) -> Option[ka])

    


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: Maybe[ka, a], k: a -> Option[ka]): Option[ka] =
        let Maybe(ma1) = ma;
        ma1(k)



    ///
    /// Runs `ma` returning an `Option`.
    ///
    pub def runMaybe(ma: Maybe[a, a]): Option[a] =
        let Maybe(ma1) = ma;
        ma1(a -> Some(a))


    instance Functor[Maybe[ka]] {
        pub def map(f: a -> b & ef, ma: Maybe[ka, a]): Maybe[ka, b] & ef =
            Maybe(k ->
                apply1(ma, v1 -> k(f(v1) as & Pure))
            ) as & ef
    }


    instance Applicative[Maybe[ka]] {
        pub def point(x: a): Maybe[ka, a] = Maybe(k -> k(x))

        pub def ap(mf: Maybe[ka, a -> b & ef], ma: Maybe[ka, a]): Maybe[ka, b] & ef =
            Maybe(k ->
                apply1(mf, f1 ->
                    apply1(ma, a1 ->
                        k(f1(a1) as & Pure)))
            ) as & ef
    }


    instance Monad[Maybe[ka]] {
        pub def flatMap(f: a -> Maybe[ka, b] & ef, ma: Maybe[ka, a]): Maybe[ka, b] & ef =
            flatMap(f, ma)
    }

    pub def flatMap(f: a -> Maybe[ka, b] & ef, ma: Maybe[ka,a]): Maybe[ka, b] & ef =
        Maybe(k ->
            apply1(ma, a1 ->
                apply1(f(a1) as & Pure, b1 ->
                    k(b1)))
        ) as & ef
  


}
