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

pub lawless class Bifunctor[f: Type -> Type -> Type] {

    pub def bimap(f: a -> c & ef1, g: b -> d & ef2, x: f[a, b]): f[c, d] & (ef1 and ef2)

}


instance Bifunctor[Result] {

    pub def bimap(f: a -> c & ef1, g: e -> e1 & ef2, x: Result[a, e]): Result[c, e1] & (ef1 and ef2) = 
        match x {
            case Err(e) => Err(g(e))
            case Ok(a)  => Ok(f(a))
        }
}

namespace Bifunctor {
    
    pub def lmap(f: a -> c & ef, x: f[a, b]): f[c, b] & ef with Bifunctor[f] = 
        Bifunctor.bimap(f, identity, x)

    pub def rmap(f: b -> d & ef, x: f[a, b]): f[a, d] & ef with Bifunctor[f] = 
        Bifunctor.bimap(identity, f, x)

}