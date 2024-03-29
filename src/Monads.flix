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

    pub enum Id[a](a) with ToString

    pub enum Exception[i, a](Result[a, i])

    /// ??? How to deal with effects? - don't float them up to the top level.

    pub enum Reader[i, a](i -> a)

    pub enum Writer[i, a]((i, a))

    pub enum State[i, a](i -> (i, a))

    pub enum Cont[i, a](a -> i -> i)

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


    
    instance Functor[Reader[i]] { 
        pub def map(f: a -> b & ef, x: Reader[i, a]): Reader[i, b] & ef = 
            let Reader(f1) = x;
            Reader(r -> {
                let a = f1(r); 
                f(a) as & Pure
            }) as & ef
    }


    instance Functor[State[i]] { 
        pub def map(f: a -> b & ef, x: State[i, a]): State[i, b] & ef = 
            let State(f1) = x;
            State(s -> {
                let (s1, a) = f1(s); 
                (s1, f(a)) as & Pure
            }) as & ef
    }


    instance Applicative[Id] { 
        pub def point(x: a): Id[a] = Id(x)

        pub def ap(f: Id[a -> b & ef], x: Id[a]): Id[b] & ef = 
            let Id(f1) = f;
            let Id(x1) = x;
            Id(f1(x1))
    }
    
    instance Applicative[Exception[i]] { 
        pub def point(x: a): Exception[i, a] = Exception(Ok(x))

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

    instance Applicative[Writer[i]] with Monoid[i] { 
        pub def point(x: a): Writer[i, a] = Writer((Monoid.empty(), x))

        pub def ap(f: Writer[i, a -> b & ef], x: Writer[i, a]): Writer[i, b] & ef = 
            let Writer((w1, f1)) = f;
            let Writer((w2, x1)) = x;
            Writer((Monoid.combine(w1, w2), f1(x1)))
    }


    instance Applicative[Reader[i]] { 
        pub def point(x: a): Reader[i, a] = Reader(_ -> x)

        pub def ap(f: Reader[i, a -> b & ef], x: Reader[i, a]): Reader[i, b] & ef = 
            let Reader(f1) = f;
            let Reader(x1) = x;
            Reader(r -> {
                let f2 = f1(r);
                let a = x1(r);
                f2(a) as & Pure
            }) as & ef
    }


    instance Applicative[State[i]] { 
        pub def point(x: a): State[i, a] = State(s -> (s, x))

        pub def ap(f: State[i, a -> b & ef], x: State[i, a]): State[i, b] & ef = 
            let State(f1) = f;
            let State(x1) = x;
            State(s -> {
                let (s1, f2) = f1(s);
                let (s2, a) = x1(s1);
                (s2, f2(a)) as & Pure
            }) as & ef
    }

    /// instance Alternative[Exception[i]] with Monoid[i]{ 
    ///     pub def zero(): Exception[i, a] = Exception(Err(Monoid.empty()))

    ///     pub def alt(x: Exception[i, a], y: Exception[i, a]): Exception[i, a] =
    ///         let Exception(x1) = x;
    ///         match x1 { 
    ///             case Ok(a1) => Exception(Ok(a1))
    ///             case Err(_) => y
    ///         }
    /// }


    instance Monad[Id] { 
        pub def flatMap(k: a -> Id[b] & ef, x: Id[a]): Id[b] & ef = 
            let Id(x1) = x;
            k(x1)
    }

    instance Monad[Exception[i]] { 
        pub def flatMap(k: a -> Exception[i, b] & ef, x: Exception[i, a]): Exception[i, b] & ef = 
            let Exception(x1) = x;
            match x1 { 
                case Ok(a) => k(a)
                case Err(e) => Exception(Err(e))
            }
    }

    instance Monad[Writer[i]] with Monoid[i] { 
        pub def flatMap(k: a -> Writer[i, b] & ef, x: Writer[i, a]): Writer[i, b] & ef = 
            let Writer((w1, x1)) = x;
            let Writer((w2, b)) = k(x1);
            Writer((Monoid.combine(w1, w2), b))
    }



    instance Monad[Reader[i]] { 
        pub def flatMap(k: a -> Reader[i, b] & ef, x: Reader[i, a]): Reader[i, b] & ef = 
            let Reader(x1) = x;
            Reader(r -> {
                let a = x1(r);
                let Reader(f) = k(a) as & Pure;
                f(r) 
            }) as & ef
    }

    instance Monad[State[i]] { 
        pub def flatMap(k: a -> State[i, b] & ef, x: State[i, a]): State[i, b] & ef = 
            let State(x1) = x;
            State(s -> {
                let (s1, a) = x1(s);
                let State(f) = k(a) as & Pure;
                f(s1) 
            }) as & ef
    }

    /// For `flatMap` monads should be in their own namespaces...
    /// pub def flatMapState(k: a -> State[i, b] & ef, x: State[i, a]): State[i, b] & ef = Monad.bind(x, k)


    pub def tell(x: i): Writer[i, Unit] = Writer(x, ())

    pub def get(): State[i, i] = State(s -> (s, s))

    pub def put(x: i): State[i, Unit] = State(_ -> (x, ()))


    /// Run functions

    pub def runId(x: Id[a]): a = 
        let Id(x1) = x;
        x1

    pub def runException(x: Exception[i, a]): Result[a, i] = 
        let Exception(x1) = x;
        x1


    pub def runReader(x: Reader[i, a], r: i): a = 
        let Reader(x1) = x;
        x1(r)

    pub def runWriter(x: Writer[i, a]): (i, a) = 
        let Writer(x1) = x;
        x1

    pub def execWriter(x: Writer[i, a]): i = 
        let Writer(x1) = x;
        fst(x1)


    pub def runState(x: State[i, a], s: i): (i, a) = 
        let State(x1) = x;
        x1(s)

    pub def execState(x: State[i, a], s: i): i = 
        let State(x1) = x;
        fst(x1(s))

    pub def evalState(x: State[i, a], s: i): a = 
        let State(x1) = x;
        snd(x1(s))


}
