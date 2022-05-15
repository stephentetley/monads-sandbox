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
pub class Alt[m: Type -> Type] with Applicative[m] {

    pub def alt(x: m[a], y: m[a]): m[a] 


}


///
/// A type class for moniodal applicative functors biased to "choice" / "alternative".
///
pub class Alternative[m: Type -> Type] with Alt[m] {

    pub def zero(): m[a]

    pub def alt(x: m[a], y: m[a]): m[a] = Alt.alt(x, y)

}

instance Alt[List] {
    pub def alt(x: List[a] , y: List[a]): List[a] = List.append(x,y)
}

instance Alternative[List] {
    pub def zero(): List[a] = Nil
}

instance Alt[Option] { 
    pub def alt(x: Option[a], y: Option[a]): Option[a] = 
        match x { 
            case Some(x1) => Some(x1)
            case None => y
        }
}


instance Alternative[Option] { 
    pub def zero(): Option[a] = None
}

namespace Alternative {
    use Applicative.{<&>, point};
    use Alternative.{alt, zero};

    pub def <|>(x: m[a] , y: m[a]): m[a] with Alternative[m] = 
        alt(x, y)
    
    pub def guard(x: Bool): m[Unit] with Alternative[m] = 
        if (x) point(()) else zero()

    /// `foldLeft` or `foldRight`?
    pub def asum(xs: t[m[a]]): m[a] with Alternative[m], Foldable[t] = 
        Foldable.foldRight(alt, zero(), xs)


    ///  One or none.
    pub def optional(x: m[a]): m[Option[a]] with Alternative[m] =
        alt(Functor.map(x1 -> Some(x1), x), point(None))

    pub def withDefault(v: m[a], d: a): m[a] with Alternative[m] =
        alt(v, point(d))

/*
    pub def choice[a, m :# Type -> Type: Alternative](xs: List[m[a]]): m[a] = 
        List.foldRight(alt, zero(), xs)

*/

}