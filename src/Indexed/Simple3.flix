/*
 * Copyright 2022 Stephen Tetley
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

/// See for instance:

/// https://hackage.haskell.org/package/indexed
/// https://kseo.github.io/posts/2017-01-12-indexed-monads.html

pub class IxFunctor[m: Bool -> Type -> Type] {               
    pub def imap(f: a -> b \ ef1, x: m[ef, a]): m[ef and ef1, b]
}

pub class IxApplicative[m: Bool -> Type -> Type] {
    pub def ipoint(x: a): m[true, a]

    pub def iap(f: m[efi, a -> b \ ef1], x: m[efj, a]): m[efi and efj and ef1, b]
}

pub class IxMonad[m: Bool -> Type -> Type] {
    pub def iflatMap(f: a -> m[efj, b] \ ef1, x: m[efi, a]): m[efi and efj and ef1, b]
}

namespace Indexed/Simple3 {


    pub enum IxState[s: Type, ef: Bool, a: Type](s -> (s, a) \ ef)

    instance IxFunctor[IxState[s]] {
        pub def imap(f: a -> b \ ef1, x: IxState[s, ef, a]): IxState[s, ef and ef1, b] = 
            map(f, x)
    }

    instance IxApplicative[IxState[s]] {
        pub def ipoint(x: a): IxState[s, true, a] = 
            point(x)

        pub def iap(f: IxState[s, efi, a -> b \ ef1], x: IxState[s, efj, a]): IxState[s, efi and efj and ef1, b] =
            ap(f, x)
    }

    instance IxMonad[IxState[s]] {
        pub def iflatMap(f: a -> IxState[s, efj, b] \ ef1, x: IxState[s, efi, a]): IxState[s, efi and efj and ef1, b] = 
            flatMap(f, x)
    }


    pub def runState(m: IxState[s, ef, a], s: s): (s, a) \ ef = 
        let IxState(f) = m;
        f(s)

    pub def point(x: a): IxState[s, true, a] = 
        IxState(s -> (s, x))

    pub def map(f: a -> b \ ef1, m: IxState[s, ef, a]): IxState[s, ef and ef1, b] = 
        IxState(s -> 
            let (s1, a) = runState(m, s);
            (s1, f(a))
        )

    pub def ap(f: IxState[s, efi, a -> b \ ef1], x: IxState[s, efj, a]): IxState[s, efi and efj and ef1, b] =
        IxState(s -> 
            let (s1, f1) = runState(f, s);
            let (s2, x1) = runState(x, s1);
            (s2, f1(x1))
        )

    pub def flatMap(f: a -> IxState[s, efj, b] \ ef1, x: IxState[s, efi, a]): IxState[s, efi and efj and ef1, b] = 
        IxState(s -> 
            let (s1, x1) = runState(x, s);
            runState(f(x1), s1)
        )

    pub def get(): IxState[s, true, s] =
        IxState(s -> 
            (s, s)
        )

    pub def put(s: s): IxState[s, true, Unit] =
        IxState(_ -> 
            (s, ())
        )

    pub def modify(f: s -> s \ ef): IxState[s, ef, Unit] =
        IxState(s -> 
            (f(s), ())
        )

    pub def liftIO(f: Unit -> a \ IO): IxState[s, false, a] =
        IxState(s -> 
            (s, f())
        )



}
