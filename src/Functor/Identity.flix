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
/// The Identity functor.
///

pub enum Identity[a] with Eq, Order, Hash, ToString {
    case Identity(a)
}

instance Functor[Identity] {
    pub def map(f: a -> b & ef, x: Identity[a]): Identity[b] & ef =
        let Identity(x1) = x;
        Identity(f(x1))
}

instance Applicative[Identity] {
    pub def point(x: a): Identity[a] = Identity(x)

    pub def ap(mf: Identity[a -> b & ef], x: Identity[a]): Identity[b] & ef =
        let Identity(mf1) = mf;
        let Identity(x1) = x;
        Identity(mf1(x1))
}

instance Monad[Identity] {
    pub def flatMap(k: a -> Identity[b] & ef, x: Identity[a]): Identity[b] & ef =
        let Identity(x1) = x;
        k(x1)
}

