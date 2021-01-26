/*
 * Copyright 2021 Stephen Tetley
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



///
/// A type class for exception handling in monads.
///
class MonadException[m :# Type -> Type] extends Monad[m] {

    pub def fail(msg: String): m[a]

    pub def catch(x: m[a], b: String -> m[a]): m[a] 

}



namespace MonadException {

    use Functor.{map};
    use Applicative.{point};

    pub def setErrM[a, m :# Type -> Type : MonadException](msg: String, mx: m[a]): m[a] = 
        catch(mx, _ -> fail(msg))

    pub def tryWithDefaultM[a, m :# Type -> Type : MonadException : Applicative](mx: m[a], x: a): m[a] = 
        catch(mx, _ -> point(x))

    pub def mtryM[a : Monoid, m :# Type -> Type : MonadException : Applicative](mx: m[a]): m[a] = 
        catch(mx, _ -> point(Monoid.empty()))

    pub def optionalM[a, m :# Type -> Type : MonadException : Applicative](mx: m[a]): m[Option[a]] = 
        catch(map(Some, mx), _ -> point(None))

    pub def attemptM[a, m :# Type -> Type : MonadException : Applicative](mx: m[a]): m[Result[a, String]] = 
        catch(map(Ok, mx), msg -> point(Err(msg)))
 
    pub def testM[a, m :# Type -> Type : MonadException : Applicative](mx: m[a]): m[Bool] = 
        catch(Functor.as(mx, true), _ -> point(false))
 
    pub def mapErrM[a, m :# Type -> Type : MonadException](f: String -> String, mx: m[a]): m[a] = 
        catch(mx, msg -> fail(f(msg)))

    pub def prefixErrM[a, m :# Type -> Type : MonadException](s: String, mx: m[a]): m[a] = 
        mapErrM(msg -> s + msg, mx)

}