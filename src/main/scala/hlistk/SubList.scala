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

  implicit def corecurseSame2[HK[_[_], _], A, L <: HListK, L2 <: HListK, E[_[_]], R <: HListK](
    implicit c: Contains.Aux[L2, HK[?[_], A], R], sub: SubList[L, R]
  ) = new SubList[HK[?[_], A] |: L, L2] {
    def apply[M[_]](e: Effects[L2, M]): Effects[HK[?[_], A] |: L, M] = {
      // type R[M[_]] = HK[M, A]
      // val ConsFX(head, tail) = e:Effects[R |: L2, M]
      val (ef, r) = c.extract(e)
      ef :: sub(r)
    }
  }
}



/*
trait LowSubList extends LowSubList2 {

  implicit def corecurseDiff11[HK[_[_]], L <: HListK, HK2[_[_]], L2 <: HListK, E[_[_]]](
    implicit sub: SubList[HK |: L, L2]
  ) = new SubList[HK |: L, HK2 |: L2] {
    def apply[M[_]](e: Effects[HK2 |: L2, M]): Effects[HK |: L, M] = {
      val ConsFX(_, tail) = e
      sub(tail)
    }
  }

  implicit def corecurseDiff12[HK[_[_]], L <: HListK, HK2[_[_], _], A, L2 <: HListK, E[_[_]]](
    implicit sub: SubList[HK |: L, L2]
  ) = new SubList[HK |: L, HK2[?[_], A] |: L2] {
    def apply[M[_]](e: Effects[HK2[?[_], A] |: L2, M]): Effects[HK |: L, M] = {
      type R[M[_]] = HK2[M, A]
      val ConsFX(_, tail) = e:Effects[R |: L2, M]
      sub(tail)
    }
  }

  implicit def corecurseDiff13[HK[_[_]], L <: HListK, HK2[_[_], _, _], A, B, L2 <: HListK, E[_[_]]](
    implicit sub: SubList[HK |: L, L2]
  ) = new SubList[HK |: L, HK2[?[_], A, B] |: L2] {
    def apply[M[_]](e: Effects[HK2[?[_], A, B] |: L2, M]): Effects[HK |: L, M] = {
      type R[M[_]] = HK2[M, A, B]
      val ConsFX(_, tail) = e:Effects[R |: L2, M]
      sub(tail)
    }
  }

}

trait LowSubList2 extends LowSubList3 {

  implicit def corecurseDiff21[HK[_[_], _], A, L <: HListK, HK2[_[_]], A2, L2 <: HListK, E[_[_]]](
    implicit sub: SubList[HK[?[_], A] |: L, L2]
  ) = new SubList[HK[?[_], A] |: L, HK2 |: L2] {
    def apply[M[_]](e: Effects[HK2 |: L2, M]): Effects[HK[?[_], A] |: L, M] = {
      val ConsFX(_, tail) = e
      sub(tail)
    }
  }

  implicit def corecurseDiff22[HK[_[_], _], A, L <: HListK, HK2[_[_], _], A2, L2 <: HListK, E[_[_]]](
    implicit sub: SubList[HK[?[_], A] |: L, L2]
  ) = new SubList[HK[?[_], A] |: L, HK2[?[_], A2] |: L2] {
    def apply[M[_]](e: Effects[HK2[?[_], A2] |: L2, M]): Effects[HK[?[_], A] |: L, M] = {
      type R[M[_]] = HK2[M, A2]
      val ConsFX(_, tail) = e:Effects[R |: L2, M]
      sub(tail)
    }
  }  

  implicit def corecurseDiff23[HK[_[_], _], A, B, L <: HListK, HK2[_[_], _, _], A2, B2, L2 <: HListK, E[_[_]]](
    implicit sub: SubList[HK[?[_], A] |: L, L2]
  ) = new SubList[HK[?[_], A] |: L, HK2[?[_], A2, B2] |: L2] {
    def apply[M[_]](e: Effects[HK2[?[_], A2, B2] |: L2, M]): Effects[HK[?[_], A] |: L, M] = {
      type R[M[_]] = HK2[M, A2, B2]
      val ConsFX(_, tail) = e:Effects[R |: L2, M]
      sub(tail)
    }
  }  
}

trait LowSubList3 {

  implicit def corecurseDiff31[HK[_[_], _, _], A, B, L <: HListK, HK2[_[_]], L2 <: HListK, E[_[_]]](
    implicit sub: SubList[HK[?[_], A, B] |: L, L2]
  ) = new SubList[HK[?[_], A, B] |: L, HK2 |: L2] {
    def apply[M[_]](e: Effects[HK2 |: L2, M]): Effects[HK[?[_], A, B] |: L, M] = {
      val ConsFX(_, tail) = e
      sub(tail)
    }
  }

  implicit def corecurseDiff32[HK[_[_], _, _], A, B, L <: HListK, HK2[_[_], _], A2, L2 <: HListK, E[_[_]]](
    implicit sub: SubList[HK[?[_], A, B] |: L, L2]
  ) = new SubList[HK[?[_], A, B] |: L, HK2[?[_], A2] |: L2] {
    def apply[M[_]](e: Effects[HK2[?[_], A2] |: L2, M]): Effects[HK[?[_], A, B] |: L, M] = {
      type R[M[_]] = HK2[M, A2]
      val ConsFX(_, tail) = e:Effects[R |: L2, M]
      sub(tail)
    }
  }  

  implicit def corecurseDiff33[HK[_[_], _, _], A, B, L <: HListK, HK2[_[_], _, _], A2, B2, L2 <: HListK, E[_[_]]](
    implicit sub: SubList[HK[?[_], A, B] |: L, L2]
  ) = new SubList[HK[?[_], A, B] |: L, HK2[?[_], A2, B2] |: L2] {
    def apply[M[_]](e: Effects[HK2[?[_], A2, B2] |: L2, M]): Effects[HK[?[_], A, B] |: L, M] = {
      type R[M[_]] = HK2[M, A2, B2]
      val ConsFX(_, tail) = e:Effects[R |: L2, M]
      sub(tail)
    }
  }  
}

*/