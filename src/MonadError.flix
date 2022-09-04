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

/// This doesn't work well - see Haskell the version with existential types for the 
/// a working version.


/// Note - it will be very difficult (impossible?) to implement functions typed as
/// per this class for CPS monads without existential types...
class MonadError[m: Type -> Type] with Monad[m] {
    pub def throwError(e: String): m[a]

    pub def catchError(m: m[a], hdl: String -> m[a] \ ef): m[a] \ ef

    pub def reifyError(m: m[a]): m[Result[a, String]]
}

class MonadErrorCPS[m: Type -> Type -> Type] {
    pub def throwError(e: String): m[ka, a] with Monad[m[ka]]

    pub def catchError(m: m[a, a], hdl: String -> m[ka, a] \ ef): m[ka, a] \ ef

    pub def reifyError(m: m[a, a]): m[a, Result[a, String]]
}


namespace MonadsSandbox/MonadError {

    
    pub def foldLeftM(f: (b, a) -> m[b] \ ef, s: b, l: List[a]): m[b] \ ef with MonadError[m] = 
        use Applicative.{point};
        use Monad.{>>=};
        use MonadError.{throwError, reifyError};
        def loop(ll, acc) = match ll {
            case Nil     => point(acc)
            case x :: xs => reifyError(f(acc, x)) >>= ans -> match ans {  /// warning this bind breaks CSP/tail call.
                case Ok(acc1) => loop(xs, acc1)
                case Err(e)   => throwError(e)
            }
        };
        loop(l, s)


    // pub def traverse(f: a -> m[Result[b, e]] \ ef, s: b, l: List[a]): m[Result[List[b], e]] \ ef = 
    //     def loop(ll, dl) = match ll {
    //         case Nil    => dl(Nil)
    //         case x :: xs => match f(d1, x) {
    //             case Ok(acc1) => loop(xs, acc1)
    //             case Err(e)   => Err(e)
    //         }
    //     };
    //     loop(l, upcast(identity))


}

namespace MonadsSandbox/Error {

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
            flatMap(f, ma)
    }

    pub def flatMap(f: a -> Error[ka, ef, b] \ ef1, ma: Error[ka, ef, a]): Error[ka, ef, b] \ ef1 =
        Error(k ->
            apply1(ma, a1 ->
                apply1(f(a1) as \ ef, b1 ->
                    k(b1)))
        ) as \ ef1
 
    pub def throwError1(err: String): Error[ka, ef, a] = 
        Error(_ -> Err(err) as \ ef)


    // pub def either(f: String -> ka \ ef1, g: a -> ka \ ef2, ma: Error[ka, ef, a]): ka \ {ef1, ef2} = 
    //     Error(k ->
    //         apply1(ma, a1 -> match k(a1) {
    //             case Ok(a)    => Ok(g(a)) as \ ef
    //             case Err(msg) => Ok(f(msg)) as \ ef
    //         })
    //     ) as \ {ef1, ef2}


    pub def catchError1(ma: Error[a, ef, a], handler: String -> Error[ka, ef, a] \ ef1): Error[ka, ef, a] \ ef1 = 
        Error(k -> match runError(ma) { 
                    case Ok(b1) => k(b1)
                    case Err(e) => apply1(handler(e) as \ ef, b1 -> k(b1))
                }) as \ ef1

    pub def reifyError1(ma: Error[ka, ef, ka]): Error[ka, ef, Result[ka, String]] = 
        Error(k -> runError(ma) |> ans -> k(ans))


    // instance MonadError[Error[ka, ef]] {
    //     pub def throwError(err: String): Error[ka, ef, a] = throwError1(err)

    //     pub def catchError(m: Error[ka, ef, ka], hdlr: String -> Error[ka, ef, ka] \ ef1): Error[ka, ef, ka] \ ef1 = catchError1(m, hdlr)

    //     pub def reifyError(m: Error[ka, ef, ka]): Error[Result[ka, String], ef, ka] = reifyError1(m)
    // }

}

