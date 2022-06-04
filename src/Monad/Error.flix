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



namespace MonadLib/Monad/Error {

    ///
    /// Error is the codensity version of the Either monad.
    ///
    pub enum Error[ka, e, a]((a -> Result[ka, e]) -> Result[ka, e])

    


    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: Error[ka, e, a], k: a -> Result[ka, e]): Result[ka, e] =
        let Error(ma1) = ma;
        ma1(k)


    ///
    /// Runs `ma` returning a `Result`.
    ///
    pub def runError(ma: Error[a, e, a]): Result[a, e] =
        let Error(ma1) = ma;
        ma1(a -> Ok(a))


    instance Functor[Error[ka, e]] {
        pub def map(f: a -> b & ef, ma: Error[ka, e, a]): Error[ka, e, b] & ef =
            Error(k ->
                apply1(ma, v1 -> k(f(v1) as & Pure))
            ) as & ef
    }


    instance Applicative[Error[ka, e]] {
        pub def point(x: a): Error[ka, e, a] = Error(k -> k(x))

        pub def ap(mf: Error[ka, e, a -> b & ef], ma: Error[ka, e, a]): Error[ka, e, b] & ef =
            Error(k ->
                apply1(mf, f1 ->
                    apply1(ma, a1 ->
                        k(f1(a1) as & Pure)))
            ) as & ef
    }


    instance Monad[Error[ka, e]] {
        pub def flatMap(f: a -> Error[ka, e, b] & ef, ma: Error[ka, e, a]): Error[ka, e, b] & ef =
            flatMap(f, ma)
    }

    pub def flatMap(f: a -> Error[ka, e, b] & ef, ma: Error[ka, e, a]): Error[ka, e, b] & ef =
        Error(k ->
            apply1(ma, a1 ->
                apply1(f(a1) as & Pure, b1 ->
                    k(b1)))
        ) as & ef
  
    pub def throwError(err: e): Error[ka, e, a] = 
        Error(_ -> Err(err))


    pub def either(f: e -> b & ef1, g: a -> b & ef2, ma: Error[a, e, a]): b & (ef1 and ef2) = 
        match runError(ma) { 
            case Err(e) => f(e)
            case Ok(a) => g(a)
        }

    pub def catchError(ma: Error[a, e, a], handler: e -> Error[a, e1, a] & ef): Error[a, e1, a] & ef = 
        either(handler, Applicative.point, ma)


}
