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



namespace MonadLib/Monad/ContException {

    ///
    /// ContException is the codensity version of the Either monad.
    ///
    pub opaque type ContException[ka, e, a] = (a -> Result[ka, e] & Impure) -> Result[ka, e] & Impure

    


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: ContException[ka, e, a], k: a -> Result[ka, e] & Impure): Result[ka, e] & Impure =
        let ContException(ma1) = ma;
        ma1(k)


    ///
    /// Runs `ma` returning a `Result`.
    ///
    pub def runContException(ma: ContException[a, e, a]): Result[a, e] & Impure =
        let ContException(ma1) = ma;
        ma1(a -> Ok(a) as & Impure)


    instance Functor[ContException[ka, e]] {
        pub def map(f: a -> b & ef, ma: ContException[ka, e, a]): ContException[ka, e, b] & ef =
            ContException(k ->
                apply1(ma, v1 -> k(f(v1) as & Impure))
            ) as & ef
    }


    instance Applicative[ContException[ka, e]] {
        pub def point(x: a): ContException[ka, e, a] = ContException(k -> k(x))

        pub def ap(mf: ContException[ka, e, a -> b & ef], ma: ContException[ka, e, a]): ContException[ka, e, b] & ef =
            ContException(k ->
                apply1(mf, f1 ->
                    apply1(ma, a1 ->
                        k(f1(a1) as & Impure)))
            ) as & ef
    }


    instance Monad[ContException[ka, e]] {
        pub def flatMap(f: a -> ContException[ka, e, b] & ef, ma: ContException[ka, e, a]): ContException[ka, e, b] & ef =
            flatMap(f, ma)
    }

    pub def flatMap(f: a -> ContException[ka, e, b] & ef, ma: ContException[ka, e, a]): ContException[ka, e, b] & ef =
        ContException(k ->
            apply1(ma, a1 ->
                apply1(f(a1) as & Impure, b1 ->
                    k(b1)))
        ) as & ef
  
    pub def throwError(err: e): ContException[ka, e, a] = 
        ContException(_ -> Err(err) as & Impure)


    pub def either(f: e -> b & ef1, g: a -> b & ef2, ma: ContException[a, e, a]): b & Impure = 
        match runContException(ma) { 
            case Err(e) => f(e) as & Impure
            case Ok(a) => g(a) as & Impure
        }

    pub def catchError(ma: ContException[a, e, a], handler: e -> ContException[a, e1, a] & ef): ContException[a, e1, a] & ef= 
        either(handler, Applicative.point, ma) as & ef


}
