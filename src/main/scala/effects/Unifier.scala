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


trait Unifier[Ops[_[_]], Ops2[_[_]]] {
  type Ops3[_[_]]

  def down[M[_]](e: Ops3[M]): Ops[M]
  def down2[M[_]](e: Ops3[M]): Ops2[M]
}


object Unifier extends LowerUnifier {
  type Aux[Ops0[_[_]], Ops20[_[_]], Ops30[_[_]]] = Unifier[Ops0, Ops20] {
    type Ops3[m[_]] = Ops30[m]
  }

  implicit def unifierSame[L <: HListK]: Unifier.Aux[Effects[L, ?[_]], Effects[L, ?[_]], Effects[L, ?[_]]] = 
    new Unifier[Effects[L, ?[_]], Effects[L, ?[_]]] {
      type Ops3[M[_]] = Effects[L, M]
      def down[M[_]](e: Effects[L, M]): Effects[L, M] = e
      def down2[M[_]](e: Effects[L, M]): Effects[L, M] = e
    }

}

trait LowerUnifier {

  // implicit def unifierSub[L <: HListK, L2 <: HListK, L3 <: HListK](
  //   implicit s1: SubList[L, L3], s2: SubList[L2, L3]
  // ): Unifier.Aux[Effects[L, ?[_]], Effects[L2, ?[_]], Effects[L3, ?[_]]] = 
  //   new Unifier[Effects[L, ?[_]], Effects[L2, ?[_]]] {
  //     type Ops3[M[_]] = Effects[L3, M]
  //     def down[M[_]](e: Effects[L3, M]): Effects[L, M] = s1(e)
  //     def down2[M[_]](e: Effects[L3, M]): Effects[L2, M] = s2(e)
  //   }

  implicit def unifierSub[L <: HListK, L2 <: HListK, L3 <: HListK](
    implicit ap: Append.Aux[L, L2, L3], s1: SubList[L, L3], s2: SubList[L2, L3]
  ): Unifier.Aux[Effects[L, ?[_]], Effects[L2, ?[_]], Effects[L3, ?[_]]] = 
    new Unifier[Effects[L, ?[_]], Effects[L2, ?[_]]] {
      type Ops3[M[_]] = Effects[L3, M]
      def down[M[_]](e: Effects[L3, M]): Effects[L, M] = s1(e)
      def down2[M[_]](e: Effects[L3, M]): Effects[L2, M] = s2(e)
    }

  // implicit def unifierSub[L <: HListK, L2 <: HListK, L3 <: HListK, L4 <: HListK](
  //   implicit ap: Append.Aux[L, L2, L3], s1: SubList[L, L3], s2: SubList[L2, L3], IsoK: IsoK[L3, L4]
  // ): Unifier.Aux[Effects[L, ?[_]], Effects[L2, ?[_]], Effects[L4, ?[_]]] = 
  //   new Unifier[Effects[L, ?[_]], Effects[L2, ?[_]]] {
  //     type Ops3[M[_]] = Effects[L4, M]
  //     def down[M[_]](e: Effects[L4, M]): Effects[L, M] = s1(IsoK.from(e))
  //     def down2[M[_]](e: Effects[L4, M]): Effects[L2, M] = s2(IsoK.from(e))
  //   }
}

trait IsoOps[Ops[_[_]], Ops2[_[_]]] {
  def from[M[_]](l2: Ops2[M]): Ops[M]
  def to[M[_]](l:Ops[M]): Ops2[M]
}

object IsoOps {
  implicit def isoOps[L <: HListK, L2 <: HListK](
    implicit iso: IsoK[L, L2]
  ): IsoOps[Effects[L, ?[_]], Effects[L2, ?[_]]] = new IsoOps[Effects[L, ?[_]], Effects[L2, ?[_]]] {
    def from[M[_]](l2: Effects[L2, M]): Effects[L, M] = iso.from(l2)
    def to[M[_]](l:Effects[L, M]): Effects[L2, M] = iso.to(l)
  }
}
