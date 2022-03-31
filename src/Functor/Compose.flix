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
/// The Compose functor.
///


// This passes kinding but causes a crash in the Finalizer.
// see also Test.Kind.Enum

pub enum Compose[f : Type -> Type, g : Type -> Type, a : Type] {
    case Compose(f[g[a]])
}

/*
instance Eq[Compose[f, g, a]] with Eq[a], Eq[f[a]], Eq[g[a]] {
    pub def eq(x: Compose[f, g, a], y: Compose[f, g, a]): Bool = match (x, y) {
        case (Compose(x1), Compose(y1)) => a -> x1(a) == y1(a)
    }
}
*/

instance Functor[Compose[f, g]] with Functor[f], Functor[g] {
    pub def map(f: a -> b & ef, x: Compose[f, g, a]): Compose[f, g, b] & ef =
        let Compose(x1) = x;
        Compose(Functor.map(Functor.map(f), x1))
}


instance Applicative[Compose[f, g]] with Applicative[f], Applicative[g] {
    pub def point(x: a): Compose[f, g, a] = Compose(Applicative.point(Applicative.point(x)))

    pub def ap(mf: Compose[f, g, a -> b & ef], mx: Compose[f, g, a]): Compose[f, g, b] & ef =
        let Compose(mf1) = mf;
        let Compose(mx1) = mx;
        Compose((Applicative.ap `Functor.map` mf1) `Applicative.ap` mx1)
}


