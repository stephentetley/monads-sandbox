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
/// A type class for monads.
///
class Monad[m :# Type -> Type] extends Applicative[m] {

    pub def bind(x: m[a], k: a -> m[b]): m[b]

}


instance Monad[List] {
    def bind(f: List[a], k: a -> List[b]): List[b] = List.flatMap(k, f)
}

instance Monad[Option] {
    def bind(f: Option[a], k: a -> Option[b]): Option[b] = Option.flatMap(k, f)
}


namespace Monad {

    use Monad.{bind};

    pub def >>=[a, b, m :# Type -> Type : Monad](x: m[a], k: a -> m[b]): m[b] =
        bind(x, k)

    pub def =<<[a, b, m :# Type -> Type : Monad](k: a -> m[b], x: m[a]): m[b] =
        bind(x, k)


    pub def kleisliLeft[a, b, c, m :# Type -> Type : Monad](f1: a -> m[b], f2: b -> m[c]): a -> m[c] = x ->
        bind(f1(x), x1 -> f2(x1))

    pub def kleisliRight[a, b, c, m :# Type -> Type : Monad](f1: b -> m[c], f2: a -> m[b]): a -> m[c] = x -> 
        bind(f2(x), x1 -> f1(x1))      
    
    /// Alias for `kleisliLeft`
    pub def >=>[a, b, c, m :# Type -> Type : Monad](f1: a -> m[b], f2: b -> m[c]): a -> m[c] = x ->
        kleisliLeft(f1, f2, x)

    /// Alias for `kleisliRight`
    pub def <=<[a, b, c, m :# Type -> Type : Monad](f1: b -> m[c], f2: a -> m[b]): a -> m[c] = x -> 
        kleisliRight(f1, f2, x)

    pub def whenTrueM[a, m :# Type -> Type : Monad](test: m[Bool], ma: m[a]): m[Option[a]] = 
        bind(test, x -> Applicative.whenTrue(x, ma))

    pub def whenFalseM[a, m :# Type -> Type : Monad](test: m[Bool], ma: m[a]): m[Option[a]] = 
        bind(test, x -> Applicative.whenFalse(x, ma))

}