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



namespace MonadsSandbox/MonadicTraverse {

    /// The implementation of `flatMap` for the Monad instance must be stack safe.
    /// If the Monad instance supports failure the traversal short circuits on the first failure.
    pub def traverseM(f: a -> m[b] \ ef, l: List[a]): m[List[b]] \ ef with Monad[m] = 
        use Monad.{>>=};
        def loop(ll, k) = match ll {
            case Nil => k(Nil)
            case x :: xs => f(x) >>= (a1 -> loop(xs, ks -> k(a1 :: ks)))
        };
        loop(l, upcast(Applicative.point))

}

namespace MonadsSandbox/ErrorMonad {

    ///
    /// Error is the codensity version of the Either monad.
    ///
    pub enum Error[ka: Type, ef: Bool, a: Type]((a -> Result[ka, String] \ ef) -> Result[ka, String] \ef)

    

    ///
    /// Helper function for the implementation of `map` and `ap` etc.
    ///
    def apply1(ma: Error[ka, ef, a], k: a -> Result[ka, String] \ ef): Result[ka, String] \ ef=
        let Error(ma1) = ma;
        ma1(k)



    ///
    /// Runs `ma` returning a `Result`.
    ///
    pub def runError(ma: Error[a, ef, a]): Result[a, String] \ ef =
        let Error(ma1) = ma;
        ma1(a -> Ok(a) as \ ef)


    instance Functor[Error[ka, ef]] {
        pub def map(f: a -> b \ ef1, ma: Error[ka, ef, a]): Error[ka, ef, b] \ ef1 =
            Error(k ->
                apply1(ma, v1 -> k(f(v1) as \ ef))
            ) as \ ef1
    }


    instance Applicative[Error[ka, e]] {
        pub def point(x: a): Error[ka, ef, a] = Error(k -> k(x))

        pub def ap(mf: Error[ka, ef, a -> b \ ef1], ma: Error[ka, ef, a]): Error[ka, ef, b] \ ef1 =
            Error(k ->
                apply1(mf, f1 ->
                    apply1(ma, a1 ->
                        k(f1(a1) as \ ef)))
            ) as \ ef1
    }


    instance Monad[Error[ka, ef]] {
        pub def flatMap(f: a -> Error[ka, ef, b] \ ef1, ma: Error[ka, ef, a]): Error[ka, ef, b] \ ef1 =
            flatMapImpl(f, ma)
    }

    pub def flatMapImpl(f: a -> Error[ka, ef, b] \ ef1, ma: Error[ka, ef, a]): Error[ka, ef, b] \ ef1 =
        Error(k ->
            apply1(ma, a1 ->
                apply1(f(a1) as \ ef, b1 ->
                    k(b1)))
        ) as \ ef1
 
    pub def throwError(err: String): Error[ka, ef, a] = 
        Error(_ -> Err(err) as \ ef)




}

