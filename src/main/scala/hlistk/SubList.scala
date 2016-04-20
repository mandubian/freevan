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


trait SubList[L <: HListK, L2 <: HListK] {
  def apply[M[_]](l: Effects[L2, M]): Effects[L, M]
}

object SubList /*extends LowSubList*/ {

  implicit def nil[L <: HListK] = new SubList[HNilK, L] {
    def apply[M[_]](l: Effects[L, M]): Effects[HNilK, M] = NoFX[M]
  }

  implicit def corecurseSame[HK[_[_]], L <: HListK, L2 <: HListK, E[_[_]], R <: HListK](
    implicit c: Contains.Aux[L2, HK, R], sub: SubList[L, R]
  ) = new SubList[HK |: L, L2] {
    def apply[M[_]](e: Effects[L2, M]): Effects[HK |: L, M] = {
      // val ConsFX(head, tail) = e
      val (ef, r) = c.extract(e)
      ef :: sub(r)
    }
  }

}
