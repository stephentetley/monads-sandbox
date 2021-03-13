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



namespace MonadLib/Monads {

    pub opaque type Id[a] = a

    pub opaque type Exception[i, a] = Result[a, i]

    /// ??? How to deal with effects?
    pub enum Reader[ef :# Bool, i, a] { 
        case Reader(i -> a & ef)
    }

    pub opaque type Writer[i, a] = (i, a)

    pub opaque type State[i, a] = i -> (i, a)

    pub opaque type Cont[i, a] = a -> i -> i

    /// Instances

    instance Functor[Id] { 
        pub def map(f: a -> b & ef, x: Id[a]): Id[b] & ef = 
            let Id(x1) = x;
            Id(f(x1))
    
    }

    instance Functor[Exception[i]] { 
        pub def map(f: a -> b & ef, x: Exception[i, a]): Exception[i, b] & ef =
            let Exception(x1) = x;
            match x1 { 
                case Ok(a) => {let b = f(a); Exception(Ok(b))}
                case Err(e) => Exception(Err(e)) as & ef
            }
    }

    instance Functor[Writer[i]] { 
        pub def map(f: a -> b & ef, x: Writer[i, a]): Writer[i, b] & ef =
            let Writer((w, a)) = x;
            Writer((w, f(a)))
    }



    /// pub def mapReader[a, b, i, ef :# Bool](f: a -> b & ef, x: Reader[ef, i, a]): Reader[ef, i, b] = 
    ///     match x { 
    ///         case Reader(f1) => { 
    ///             Reader(r -> {let a = f1(r); f(a)})
    ///         }
    ///     }
    
    /// instance Functor[Reader[i, Impure]] { 
    ///     def map(f: a -> b & ef, x: Reader[i, Impure, a]): Reader[i, Impure, b] & Impure = 
    ///         let Reader(f1) = x;
    ///         Reader(r -> {
    ///             let a = f1(r); 
    ///             f(a) as & Impure
    ///         })
    /// }


    instance Applicative[Id] { 
        pub def return(x: a): Id[a] = Id(x)

        pub def ap(f: Id[a -> b & ef], x: Id[a]): Id[b] & ef = 
            let Id(f1) = f;
            let Id(x1) = x;
            Id(f1(x1))
    }
    
    instance Applicative[Exception[i]] { 
        pub def return(x: a): Exception[i, a] = Exception(Ok(x))

        pub def ap(f: Exception[i, a -> b & ef], x: Exception[i, a]): Exception[i, b] & ef =
            let Exception(f1) = f;            
            match f1 { 
                case Ok(g) => { 
                    let Exception(x1) = x;
                    match x1 {
                        case Ok(y) => {let b = g(y); Exception(Ok(b))}
                        case Err(e2) => Exception(Err(e2)) as & ef
                    }
                }
                case Err(e1) => Exception(Err(e1)) as & ef
            }
    }

    instance Applicative[Writer[i]] with [i : Monoid] { 
        pub def return(x: a): Writer[i, a] = Writer((Monoid.empty(), x))

        pub def ap(f: Writer[i, a -> b & ef], x: Writer[i, a]): Writer[i, b] & ef = 
            let Writer((w1, f1)) = f;
            let Writer((w2, x1)) = x;
            Writer((Monoid.combine(w1, w2), f1(x1)))
    }


    /// instance Applicative[Reader] { 
    ///     def return(x: a): Reader[i, a] = Reader(_ -> x)

    ///     def ap(f: Reader[i, a -> b & ef], x: Reader[i, a]): Reader[i, b] & ef = 
    ///         let Reader(f1) = f;
    ///         let Reader(x1) = x;
    ///         Reader(r -> {
    ///             let f = f1(r);
    ///             let a = x1(r);
    ///             f(a)
    ///         })
    /// }


    instance Alternative[Exception[i]] with [i : Monoid]{ 
        pub def zero(): Exception[i, a] = Exception(Err(Monoid.empty()))

        pub def alt(x: Exception[i, a], y: Exception[i, a]): Exception[i, a] =
            let Exception(x1) = x;
            match x1 { 
                case Ok(a1) => Exception(Ok(a1))
                case Err(_) => y
            }
    }


    instance Monad[Id] { 
        pub def bind(x: Id[a], k: a -> Id[b]): Id[b] = 
            let Id(x1) = x;
            k(x1)
    }

    instance Monad[Exception[i]] { 
        pub def bind(x: Exception[i, a], k: a -> Exception[i, b]): Exception[i, b] = 
            let Exception(x1) = x;
            match x1 { 
                case Ok(a) => k(a)
                case Err(e) => Exception(Err(e))
            }
    }

    instance Monad[Writer[i]] with [i : Monoid] { 
        pub def bind(x: Writer[i, a], k: a -> Writer[i, b]): Writer[i, b] = 
            let Writer((w1, x1)) = x;
            let Writer((w2, b)) = k(x1);
            Writer((Monoid.combine(w1, w2), b))
    }



    /// instance Monad[Reader] { 
    ///     def bind(x: Reader[i, a], k: a -> Reader[i, b]): Reader[i, b] = 
    ///         let Reader(x1) = x;
    ///         Reader(r -> {
    ///             let a = x1(r);
    ///             k(a)
    ///         })

    /// }

    /// Run functions

    pub def runId(x: Id[a]): a = 
        let Id(x1) = x;
        x1

    pub def runException(x: Exception[i, a]): Result[a, i] = 
        let Exception(x1) = x;
        x1


    /// pub def runReader(r: i, x: Reader[ef, i, a]): a & ef = 
    ///     let Reader(x1) = x;
    ///     x1(r)

    pub def runWriter(x: Writer[i, a]): (i, a) = 
        let Writer(x1) = x;
        x1


}
