/*
Copyright 2015 Pascal Voitot

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
package freevan
package effects

trait Effects[L <: HListK, M[_]] {
  def ::[HK[_[_]]](e: HK[M]): Effects[HK |: L, M] = ConsFX(e, this)
}

case class ConsFX[HK[_[_]], M[_], TK <: HListK](
  head: HK[M]
, tail: Effects[TK, M]
) extends Effects[HK |: TK, M]

case class NoFX[M[_]]() extends Effects[HNilK, M]


