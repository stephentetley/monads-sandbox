
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

pub lawless class Compactable[t: Type -> Type] {

    pub def compact(t: t[Option[a]]): t[a]

    pub def separate(t: t[Either[l, r]]): (t[l], t[r])


}


instance Compactable[Option] {

    pub def compact(x: Option[Option[a]]): Option[a] = Helper.join(x)

    pub def separate(x: Option[Either[a, b]]): (Option[a], Option[b]) = match x {
        case Some(Left(a))  => (Some(a), None)
        case Some(Right(b)) => (None, Some(b))
        case None           => (None, None)
    }
    

}

namespace Compactable {

    pub def mapOption(f: a -> Option[b] & ef, t: t[a]): t[b] & ef with Functor[t], Compactable[t] = 
        Compactable.compact(Functor.map(f, t))

    pub def mapEither(f: a -> Either[l, r] & ef, t: t[a]): (t[l], t[r]) & ef with Functor[t], Compactable[t] =
        Compactable.separate(Functor.map(f, t))

    pub def applyOption(fa: t[a -> Option[b] & ef], t: t[a]): t[b] & ef with Applicative[t], Compactable[t] =
        Compactable.compact(Applicative.ap(fa, t))

    pub def applyEither(fa: t[a -> Either[l, r] & ef], t: t[a]): (t[l], t[r]) & ef with Applicative[t], Compactable[t] =
        Compactable.separate(Applicative.ap(fa, t))

    pub def filter(f: a -> Bool & ef, t: t[a]): t[a] & ef with Functor[t], Compactable[t] = 
        mapOption(a -> if (f(a)) Some(a) else None, t)

    pub def partition(f: a -> Bool & ef, t: t[a]): (t[a], t[a]) & ef with Functor[t], Compactable[t] = 
        mapEither(a -> if (f(a)) Right(a) else Left(a), t)
}

namespace Helper {

    /// To add to Monad...
    pub def join(x: m[m[a]]): m[a] with Monad[m] = Monad.flatMap(identity, x)
}