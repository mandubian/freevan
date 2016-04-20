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



trait IsoK[L <: HListK, L2 <: HListK] {
  def from[M[_]](l2: Effects[L2, M]): Effects[L, M]
  def to[M[_]](l:Effects[L, M]): Effects[L2, M]
}

object IsoK extends LowerIsoK {

  implicit val nil: IsoK[HNilK, HNilK] = new IsoK[HNilK, HNilK] {
    def from[M[_]](l: Effects[HNilK, M]): Effects[HNilK, M] = l
    def to[M[_]](l: Effects[HNilK, M]): Effects[HNilK, M] = l
  }

}

trait LowerIsoK {
  implicit def head[L <: HListK, H[_[_]], L2 <: HListK, R <: HListK](
    implicit hCont: Contains.Aux[L2, H, R], next: IsoK[L, R]
  ): IsoK[H |: L, L2] = new IsoK[H |: L, L2] {
    def from[M[_]](l2: Effects[L2, M]): Effects[H |: L, M] = {
      val (e, r) = hCont.extract(l2)
      e :: next.from(r)
    }

    def to[M[_]](l: Effects[H |: L, M]): Effects[L2, M] = {
      val ConsFX(h, t) = l
      hCont.rebuild(h, next.to(t))
    }
  }

}

