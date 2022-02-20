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
/// A type class for moniodal applicative functors biased to "choice" / "alternative".
///
pub lawless class FunctorWithIndex[m : Type -> Type] with Functor[m] {

    pub def mapWithIndex(f: a -> Int32 -> b & ef, x: m[a]): m[b] & ef

}

instance FunctorWithIndex[Option] {
    pub def mapWithIndex(f: a -> Int32 -> b & ef, x: Option[a]): Option[b] & ef = match x {
        case Some(x1) => Some(f(x1, 0))
        case None     => None
    }
}

instance FunctorWithIndex[List] {
    pub def mapWithIndex(f: a -> Int32 -> b & ef , x: List[a]): List[b] & ef = List.mapWithIndex(f, x)
}

instance FunctorWithIndex[Chain] {
    pub def mapWithIndex(f: a -> Int32 -> b & ef , x: Chain[a]): Chain[b] & ef = Chain.mapWithIndex(f, x)
}