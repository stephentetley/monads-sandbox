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


namespace MonadLib/Transformers {

 

    /// Transformers cause the compiler to crash...
    
    /// pub opaque type IdT[m :# Type -> Type, a] = m[a]


    /// instance Functor[Id] { 
    ///     def map(f: a -> b & e, x: IdT[a]): IdT[b] & e = 
    ///         let Id(x1) = x;
    ///         Id(f(x1))
    
    /// }

    /// instance Applicative[Id] { 
    ///     def point(x: a): Id[a] = Id(x)

    ///     def ap(f: Id[a -> b & ef], x: Id[a]): Id[b] & ef = 
    ///         let Id(f1) = f;
    ///         let Id(x1) = x;
    ///         Id(f1(x1))
    /// }
    
    /// instance Monad[Id] { 
    ///     def bind(x: Id[a], k: a -> Id[b]): Id[b] = 
    ///         let Id(x1) = x;
    ///         k(x1)
    /// }

    /// pub def runIdT[a, m :# Type -> Type : Functor](x: IdT[m, a]): m[a] = 
    ///     let IdT(x1) = x;
    ///     x1

}
