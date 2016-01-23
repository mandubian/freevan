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


trait Effects[L <: HListK, M[_]] {
  def ::[E[_[_]]](e: E[M]): Effects[E |: L, M] = freevan.::(e, this)
}

case class ::[HK[_[_]], M[_], TK <: HListK](
  head: HK[M]
, tail: Effects[TK, M]
) extends Effects[HK |: TK, M]

case class NoFX[M[_]]() extends Effects[HNilK, M]

trait Contains[L <: HListK, E[_[_]]] {
  def apply[M[_]](e: Effects[L, M]): E[M]
}

object Contains extends LowContains {

  implicit def head[L <: HListK, E[_[_]]]: Contains[E |: L, E] = new Contains[E |: L, E] {
    def apply[M[_]](e: Effects[E |: L, M]): E[M] = {
      val ::(head, _) = e
      head
    }
  }

}

trait LowContains {
  implicit def corecurse[L <: HListK, H[_[_]], E[_[_]]](implicit H: Contains[L, E]): Contains[H |: L, E] = new Contains[H |: L, E] {
    def apply[M[_]](e: Effects[H |: L, M]): E[M] = {
      val ::(_, tail) = e
      H(tail)
    }
  }
}

trait SubList[L <: HListK, L2 <: HListK] {
  def apply[M[_]](l: Effects[L2, M]): Effects[L, M]
}

object SubList extends LowSubList {

  implicit def nil[L <: HListK] = new SubList[HNilK, L] {
    def apply[M[_]](l: Effects[L, M]): Effects[HNilK, M] = NoFX[M]
  }

  implicit def corecurse[HK[_[_]], L <: HListK, L2 <: HListK, E[_[_]]](
    implicit sub: SubList[L, L2]
  ) = new SubList[HK |: L, HK |: L2] {
    def apply[M[_]](e: Effects[HK |: L2, M]): Effects[HK |: L, M] = {
      val ::(head, tail) = e
      head :: sub(tail)
    }
  }

}


trait LowSubList {

  implicit def corecurse2[HK[_[_]], L <: HListK, HK2[_[_]], L2 <: HListK, E[_[_]]](
    implicit sub: SubList[HK |: L, L2]
  ) = new SubList[HK |: L, HK2 |: L2] {
    def apply[M[_]](e: Effects[HK2 |: L2, M]): Effects[HK |: L, M] = {
      val ::(_, tail) = e
      sub(tail)
    }
  }  
}