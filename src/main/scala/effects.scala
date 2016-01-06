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

trait ForAllM[E[_[_]], A] {
  def apply[M[_]](e: E[M]): M[A]
}

sealed trait FXList

sealed trait FXNil extends FXList
case object FXNil extends FXNil

trait |:[H[_[_]], L <: FXList] extends FXList

trait Effects[L <: FXList, M[_]] {

  def ::[E[_[_]]](e: E[M]): Effects[E |: L, M] = freevan.::(e, this)

}

case class ::[E[_[_]], M[_], FX <: FXList](
  head: E[M]
, tail: Effects[FX, M]
) extends Effects[E |: FX, M]

case class NoFX[M[_]]() extends Effects[FXNil, M]

trait HasFX[L <: FXList, E[_[_]]] {
  def apply[M[_]](e: Effects[L, M]): E[M]
}

object HasFX extends LowHasFX {

  implicit def head[L <: FXList, E[_[_]]]: HasFX[E |: L, E] = new HasFX[E |: L, E] {
    def apply[M[_]](e: Effects[E |: L, M]): E[M] = {
      val ::(head, _) = e
      head
    }
  }
}

trait LowHasFX {
  implicit def corecurse[L <: FXList, H[_[_]], E[_[_]]](implicit H: HasFX[L, E]): HasFX[H |: L, E] = new HasFX[H |: L, E] {
    def apply[M[_]](e: Effects[H |: L, M]): E[M] = {
      val ::(_, tail) = e
      H(tail)
    }
  }
}