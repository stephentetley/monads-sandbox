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
/// The Constant functor.
///

pub enum Constant[a, _b] with Eq, Order, Hash, ToString {
    case Constant(a)
}

instance Functor[Constant[a]] {
    pub def map(_: b -> c & ef, x: Constant[a, b]): Constant[a, c] & ef =
        let Constant(x1) = x;
        Constant(x1) as & ef
}

instance Applicative[Constant[a]] with Monoid[a] {
    pub def point(_: b): Constant[a, b] = Constant(Monoid.empty())

    pub def ap(mf: Constant[a, b -> c & ef], x: Constant[a, b]): Constant[a, c] & ef =
        let Constant(a1) = mf;
        let Constant(x1) = x;
        Constant(Monoid.combine(a1, x1)) as & ef
}

