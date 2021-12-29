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

instance Functor[RedBlackTree.RedBlackTree[k]] {
    pub def map(f: v1 -> v2 & ef, tree: RedBlackTree.RedBlackTree[k, v1]): RedBlackTree.RedBlackTree[k, v2] & ef =
        RedBlackTree.mapWithKey((_, v) -> f(v), tree)
}

instance Functor[Map[k]] {
    pub def map(f: v1 -> v2 & ef, m: Map[k, v1]): Map[k, v2] & ef =
        let Map(tree) = m;
        Map(RedBlackTree.mapWithKey((_, v) -> f(v), tree))
}


namespace FunctorExtras {

    ///
    /// Functor choice function.
    ///
    /// Perform the action `test`, if it evaluates to `true` return `m(a)`
    /// otherwise return `m(b)`.
    ///
    pub def choose(test: m[Bool], a: a, b: a): m[a] with Functor[m] =
        Functor.map(cond -> if (cond) a else b, test)


}
