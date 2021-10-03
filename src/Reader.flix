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




lawless class Applicative[m : Type -> Type] with Functor[m] {
    ///
    /// Puts `x` into a default context.
    ///
    pub def point(x: a) : m[a]

    ///
    /// Apply the function-type applicative `f` to the argument-type applicative `x`.
    ///
    pub def ap(f: m[a -> b & ef], x: m[a]) : m[b] & ef

}

lawless class Monad[m : Type -> Type] with Applicative[m] {

    pub def bind(x: m[a], k: a -> m[b] & ef): m[b] & ef

}

namespace MonadLib/Reader {

    /// ??? How to deal with effects?
    pub opaque type Reader[i, a] = i -> a

    
    instance Functor[Reader[i]] { 
        pub def map(f: a -> b & ef, x: Reader[i, a]): Reader[i, b] & ef = 
            let Reader(x1) = x;
            Reader(r1 -> {let a1 = x1(r1); f(a1) as & Pure}) as & ef
    }


    instance Applicative[Reader[i]] { 
        pub def point(x: a): Reader[i, a] = 
            Reader(_ -> x)

        pub def ap(f: Reader[i, a -> b & ef], x: Reader[i, a]) : Reader[i, b] & ef = 
            Reader(r1 -> 
                let f1 = runReader(f, r1);
                let x1 = runReader(x, r1);
                f1(x1) as & Pure
            ) as & ef

    }

    instance Monad[Reader[i]] { 


        pub def bind(mx: Reader[i, a], k: a -> Reader[i, b] & ef) : Reader[i, b] & ef = 
            Reader(r1 -> 
                let x1 = runReader(mx, r1);
                let m1 = k(x1) as & Pure;
                runReader(m1, r1)
            ) as & ef

    }

    pub def flatMap(k: a -> Reader[i, b] & ef, mx: Reader[i, a]) : Reader[i, b] & ef = Monad.bind(mx, k)

    pub def asks(f: i -> a & ef): Reader[i, a] & ef = 
        Reader(r1 -> f(r1) as & Pure) as & ef

    pub def runReader(x: Reader[i, a], r: i): a = 
        let Reader(x1) = x;
        x1(r)

    pub def return(x: a): Reader[i, a] = 
        Reader(_ -> x)

}
