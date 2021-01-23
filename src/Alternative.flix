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
class Alternative[m :# Type -> Type] extends Applicative[m] {

    pub def zero(): m[a]

    pub def alt(x: m[a] , y: m[a]): m[a] 

}


instance Alternative[List] {
    def zero(): List[a] = Nil

    def alt(x: List[a] , y: List[a]): List[a] = List.append(x,y)
}
    
instance Alternative[Option] { 
    def zero(): Option[a] = None

    def alt(x: Option[a], y: Option[a]): Option[a] = 
        match x { 
            case Some(x1) => Some(x1)
            case None => y
        }
}

namespace Alternative {
    use Applicative.{<&>, point};
    use Alternative.{alt};

    pub def <|>[a, m :# Type -> Type : Alternative](x: m[a] , y: m[a]): m[a] = 
        alt(x, y)
    
    ///  One or none.
    pub def optional[a, m :# Type -> Type : Alternative](v: m[a]): m[Option[a]] = 
        alt(Functor.map(x1 -> Some(x1), v), point(None))

    
    pub def optionalWithDefault[a, m :# Type -> Type : Alternative](v: m[a], d: a): m[a] =         
        alt(v, point(d))

    pub def choice[a, m :# Type -> Type: Alternative](xs: List[m[a]]): m[a] = 
        List.foldRight(alt, zero(), xs)
    
    /// WARNING - not tail recursive!
    pub def count[a, m :# Type -> Type: Alternative](ntimes: Int32, mx: m[a]): m[List[a]] = 
        use Applicative.{consA};
        if (ntimes <= 0)
            point(Nil)
        else
            consA(mx, count(ntimes - 1, mx))





}