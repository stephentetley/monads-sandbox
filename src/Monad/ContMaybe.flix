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



namespace MonadLib/Monad/ContMaybe {

    ///
    /// ContMaybe is the codensity version of the Maybe monad.
    ///
    pub opaque type ContMaybe[ka, a] = (a -> Option[ka] & Impure) -> Option[ka] & Impure

    


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: ContMaybe[ka, a], k: a -> Option[ka] & Impure): Option[ka] & Impure =
        let ContMaybe(ma1) = ma;
        ma1(k)



    ///
    /// Runs `ma` returning an `Option`.
    ///
    pub def runContMaybe(ma: ContMaybe[a, a]): Option[a] & Impure =
        let ContMaybe(ma1) = ma;
        ma1(a -> Some(a) as & Impure)


    instance Functor[ContMaybe[ka]] {
        pub def map(f: a -> b & ef, ma: ContMaybe[ka, a]): ContMaybe[ka, b] & ef =
            ContMaybe(k ->
                apply1(ma, v1 -> k(f(v1) as & Impure))
            ) as & ef
    }


    instance Applicative[ContMaybe[ka]] {
        pub def point(x: a): ContMaybe[ka, a] = ContMaybe(k -> k(x))

        pub def ap(mf: ContMaybe[ka, a -> b & ef], ma: ContMaybe[ka, a]): ContMaybe[ka, b] & ef =
            ContMaybe(k ->
                apply1(mf, f1 ->
                    apply1(ma, a1 ->
                        k(f1(a1) as & Impure)))
            ) as & ef
    }


    instance Monad[ContMaybe[ka]] {
        pub def flatMap(f: a -> ContMaybe[ka, b] & ef, ma: ContMaybe[ka, a]): ContMaybe[ka, b] & ef =
            flatMap(f, ma)
    }

    pub def flatMap(f: a -> ContMaybe[ka, b] & ef, ma: ContMaybe[ka,a]): ContMaybe[ka, b] & ef =
        ContMaybe(k ->
            apply1(ma, a1 ->
                apply1(f(a1) as & Impure, b1 ->
                    k(b1)))
        ) as & ef
  


}
