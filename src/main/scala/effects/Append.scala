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



trait Append[L <: HListK, L2 <: HListK] {
  type Out <: HListK

  def append[M[_]](l: Effects[L, M], l2: Effects[L2, M]): Effects[Out, M]
}

object Append extends LowerAppend {
  type Aux[L <: HListK, L2 <: HListK, L3 <: HListK] = Append[L, L2] {
    type Out = L3
  }

  implicit def hnil[H[_[_]], L2 <: HListK]: Append.Aux[HNilK, L2, L2] =
    new Append[HNilK, L2] {
      type Out = L2

      def append[M[_]](l: Effects[HNilK, M], l2: Effects[L2, M]): Effects[L2, M] = l2
    }
}

trait LowerAppend extends LowerAppend2 {

  implicit def hnil2[L <: HListK, H[_[_]]]: Append.Aux[L, HNilK, L] =
    new Append[L, HNilK] {
      type Out = L

      def append[M[_]](l: Effects[L, M], l2: Effects[HNilK, M]): Effects[L, M] = l
    }

  implicit def headContains[L <: HListK, H[_[_]], L2 <: HListK, R <: HListK, R2 <: HListK](
    implicit contH: Contains.Aux[L2, H, R], next: Append.Aux[L, R, R2]
  ): Append.Aux[H |: L, L2, H |: R2] = new Append[H |: L, L2] {
    type Out = H |: R2

    def append[M[_]](l: Effects[H |: L, M], l2: Effects[L2, M]): Effects[H |: R2, M] = {
      val ConsFX(h, t) = l
      val (e, r) = contH.extract(l2)
      e :: next.append(t, r)
    }
  }

  implicit def headContains2[L <: HListK, H[_[_], _], A, L2 <: HListK, R <: HListK, R2 <: HListK](
    implicit contH: Contains.Aux[L2, H[?[_], A], R], next: Append.Aux[L, R, R2]
  ): Append.Aux[H[?[_], A] |: L, L2, H[?[_], A] |: R2] = new Append[H[?[_], A] |: L, L2] {
    type Out = H[?[_], A] |: R2

    def append[M[_]](l: Effects[H[?[_], A] |: L, M], l2: Effects[L2, M]): Effects[H[?[_], A] |: R2, M] = {
      type R[m[_]] = H[m, A]
      val ConsFX(h, t) = l:Effects[R |: L, M]
      val (e, r) = contH.extract(l2)
      e :: next.append(t, r)
    }
  }

}

trait LowerAppend2 {

  implicit def headNotContained[L <: HListK, H[_[_]], L2 <: HListK, R <: HListK](
    implicit next: Append.Aux[L, L2, R]
  ): Append.Aux[H |: L, L2, H |: R] = new Append[H |: L, L2] {
    type Out = H |: R

    def append[M[_]](l: Effects[H |: L, M], l2: Effects[L2, M]): Effects[H |: R, M] = {
      val ConsFX(h, t) = l
      h :: next.append(t, l2)
    }
  }

  implicit def headNotContained2[L <: HListK, H[_[_], _], A, L2 <: HListK, R <: HListK](
    implicit next: Append.Aux[L, L2, R]
  ): Append.Aux[H[?[_], A] |: L, L2, H[?[_], A] |: R] = new Append[H[?[_], A] |: L, L2] {
    type Out = H[?[_], A] |: R

    def append[M[_]](l: Effects[H[?[_], A] |: L, M], l2: Effects[L2, M]): Effects[H[?[_], A] |: R, M] = {
      type R[m[_]] = H[m, A]
      val ConsFX(h, t) = l:Effects[R |: L, M]
      h :: next.append(t, l2)
    }
  }

}