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

}
