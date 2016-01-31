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


trait UnapplyK[TCA, M[_]] {
  type TC[_[_]]

  def subst(tca: TCA): TC[M]
}

object UnapplyK extends LowerUnapplyK {

  type Aux[TCA0, TC0[_[_]], M0[_]] = UnapplyK[TCA0, M0] { type TC[m[_]] = TC0[m] }

  implicit def one[TC0[_[_]], M0[_]]: UnapplyK.Aux[TC0[M0], TC0, M0] = new UnapplyK[TC0[M0], M0] {
    type TC[m[_]] = TC0[m]

    def subst(tca: TC0[M0]): TC[M0] = tca
  }

}

trait LowerUnapplyK {
  implicit def two[TC0[_[_], _], M0[_], A]: UnapplyK.Aux[TC0[M0, A], TC0[?[_], A], M0] = new UnapplyK[TC0[M0, A], M0] {
    type TC[m[_]] = TC0[m, A]

    def subst(tca: TC0[M0, A]): TC[M0] = tca
  }
}

trait Effects[L <: HListK, M[_]] {
  // def ::[E[_[_]]](e: E[M]): Effects[E |: L, M] = ConsFX(e, this)
  def ::[EMA](e: EMA)(implicit unK: UnapplyK[EMA, M]): Effects[unK.TC |: L, M] = ConsFX(unK.subst(e), this)
}

case class ConsFX[HK[_[_]], M[_], TK <: HListK](
  head: HK[M]
, tail: Effects[TK, M]
) extends Effects[HK |: TK, M]

case class NoFX[M[_]]() extends Effects[HNilK, M]

trait Contains[L <: HListK, E[_[_]]] {
  type R <: HListK
  def extract[M[_]](e: Effects[L, M]): (E[M], Effects[R, M])
  def rebuild[M[_]](e: E[M], l: Effects[R, M]): Effects[L, M]
}

object Contains extends LowContains {

  type Aux[L <: HListK, E[_[_]], R0 <: HListK] = Contains[L, E] { type R = R0 }

  implicit def head[L <: HListK, E[_[_]]]: Contains.Aux[E |: L, E, L] = new Contains[E |: L, E] {
    type R = L

    def extract[M[_]](e: Effects[E |: L, M]): (E[M], Effects[L, M]) = {
      val ConsFX(head, tail) = e
      head -> tail
    }

    def rebuild[M[_]](e: E[M], l: Effects[R, M]): Effects[E |: L, M] = {
      e :: l
    }
  }

  implicit def head2[L <: HListK, E[_[_], _], A]: Contains.Aux[E[?[_], A] |: L, E[?[_], A], L] =
    new Contains[E[?[_], A] |: L, E[?[_], A]] {
      type R = L

      def extract[M[_]](e: Effects[λ[M[_] => E[M, A]] |: L, M]): (E[M, A], Effects[L, M]) = {
        type R = L
        
        type TMP[m[_]] = E[m, A]      
        val ConsFX(head, tail) = e:Effects[TMP |: L, M]
        head -> tail
      }

      def rebuild[M[_]](e: E[M, A], l: Effects[R, M]): Effects[E[?[_], A] |: L, M] = {
        e :: l
      }
    }
}

trait LowContains {

  implicit def corecurse[L <: HListK, H[_[_]], E[_[_]], R0 <: HListK](
    implicit next: Contains.Aux[L, E, R0]
  ): Contains.Aux[H |: L, E, H |: R0] = new Contains[H |: L, E] {
    type R = H |: R0

    def extract[M[_]](e: Effects[H |: L, M]): (E[M], Effects[H |: R0, M]) = {
      val ConsFX(h, t) = e
      val (ef, r) = next.extract(t)
      ef -> (h :: r)
    }

    def rebuild[M[_]](e: E[M], l: Effects[H |: R0, M]): Effects[H |: L, M] = {
      val ConsFX(h, t) = l
      h :: next.rebuild(e, t)
    }
  }

  implicit def corecurse2[L <: HListK, H[_[_]], E[_[_], _], A, R0 <: HListK](
    implicit next: Contains.Aux[L, E[?[_], A], R0]
  ): Contains.Aux[H |: L, E[?[_], A], H |: R0] = new Contains[H |: L, E[?[_], A]] {
    type R = H |: R0

    def extract[M[_]](e: Effects[H |: L, M]): (E[M, A], Effects[H |: R0, M]) = {
      val ConsFX(h, t) = e
      val (ef, r) = next.extract(t)
      ef -> (h :: r)
    }

    def rebuild[M[_]](e: E[M, A], l: Effects[H |: R0, M]): Effects[H |: L, M] = {
      val ConsFX(h, t) = l
      h :: next.rebuild(e, t)
    }
  }

  implicit def corecurse3[L <: HListK, H[_[_], _], A, E[_[_]], R0 <: HListK](
    implicit next: Contains.Aux[L, E, R0]
  ): Contains.Aux[H[?[_], A] |: L, E, H[?[_], A] |: R0] = new Contains[H[?[_], A] |: L, E] {
    type R = H[?[_], A] |: R0

    def extract[M[_]](e: Effects[H[?[_], A] |: L, M]): (E[M], Effects[H[?[_], A] |: R0, M]) = {
      type R[M[_]] = H[M, A]
      val ConsFX(h, t) = e:Effects[R |: L, M]
      val (ef, r) = next.extract(t)
      ef -> (h :: r)
    }

    def rebuild[M[_]](e: E[M], l: Effects[H[?[_], A] |: R0, M]): Effects[H[?[_], A] |: L, M] = {
      type RR[M[_]] = H[M, A]
      val ConsFX(h, t) = l:Effects[RR |: R0, M]
      h :: next.rebuild(e, t)
    }    
  }

  implicit def corecurse4[L <: HListK, H[_[_], _], A, E[_[_], _], B, R0 <: HListK](
    implicit next: Contains.Aux[L, E[?[_], B], R0]
  ): Contains.Aux[H[?[_], A] |: L, E[?[_], B], H[?[_], A] |: R0] = new Contains[H[?[_], A] |: L, E[?[_], B]] {
    type R = H[?[_], A] |: R0

    def extract[M[_]](e: Effects[H[?[_], A] |: L, M]): (E[M, B], Effects[H[?[_], A] |: R0, M]) = {
      type R[M[_]] = H[M, A]
      val ConsFX(h, t) = e:Effects[R |: L, M]
      val (ef, r) = next.extract(t)
      ef -> (h :: r)
    }

    def rebuild[M[_]](e: E[M, B], l: Effects[H[?[_], A] |: R0, M]): Effects[H[?[_], A] |: L, M] = {
      type RR[M[_]] = H[M, A]
      val ConsFX(h, t) = l:Effects[RR |: R0, M]
      h :: next.rebuild(e, t)
    }
  }
}

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
