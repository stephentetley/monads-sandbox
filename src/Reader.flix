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



namespace MonadLib/Reader {

    /// ??? How to deal with effects?
    pub opaque type Reader[i, a] = i -> a & Impure

    
    instance Functor[Reader[i]] { 
        pub def map(f: a -> b & ef, x: Reader[i, a]): Reader[i, b] & ef = 
            let Reader(x1) = x;
            Reader(r1 -> {let a1 = x1(r1); f(a1) as & Impure}) as & ef
    }


    instance Applicative[Reader[i]] { 
        pub def point(x: a): Reader[i, a] = 
            Reader(_ -> x as & Impure)

        pub def ap(f: Reader[i, a -> b & ef], x: Reader[i, a]) : Reader[i, b] & ef = 
            Reader(r1 -> 
                let f1 = runReader(f, r1);
                let x1 = runReader(x, r1);
                f1(x1) as & Impure
            ) as & ef

    }

    instance Monad[Reader[i]] { 
        pub def flatMap(k: a -> Reader[i, b] & ef, ma: Reader[i, a]) : Reader[i, b] & ef = flatMap(k, ma)
    }

    pub def flatMap(k: a -> Reader[i, b] & ef, mx: Reader[i, a]) : Reader[i, b] & ef =
        Reader(r1 -> 
            let x1 = runReader(mx, r1);
            let m1 = k(x1) as & Impure;
            runReader(m1, r1)
        ) as & ef



    pub def asks(f: i -> a & ef): Reader[i, a] & ef = 
        Reader(r1 -> f(r1) as & Impure) as & ef

    pub def runReader(x: Reader[i, a], r: i): a & Impure = 
        let Reader(x1) = x;
        x1(r)


}
