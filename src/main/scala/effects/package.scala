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

import cats.Monad


package object effects {

  type FreeVanFX[L <: HListK, A] = FreeVan[Effects[L, ?[_]], A]

  // case class FreeVanFX[L <: HListK, A](self: FreeVan[Effects[L, ?[_]], A]) extends AnyVal {
  //   // self =>

  //   // def expand[L2 <: HListK](implicit subList: SubList[L, L2]): FreeVanFX[L2, A] = 
  //   //   FreeVanFX[L2, A](new FreeVan[Effects[L2, ?[_]], A] {
  //   //     def runFree[M[_] : Monad](effs: Effects[L2, M]): M[A] = self.runFree(subList(effs))
  //   //   })

  //   def map[B](f: A => B): FreeVanFX[L, B] = FreeVanFX[L, B](self.map(f))

  //   def flatMap[B, L2 <: HListK, L3 <: HListK](f: A => FreeVanFX[L2, B])(
  //     implicit unifier: Unifier[Effects[L, ?[_]], Effects[L2, ?[_]], Effects[L3, ?[_]]]
  //   ): FreeVanFX[L3, B] =
  //     FreeVanFX[L3, B](new FreeVan[Effects[L3, ?[_]], B] {
  //       def runFree[M[_] : Monad](e: Effects[L3, M]): M[B] = Monad[M].flatMap(self.runFree(unifier.down(e)))(a => f(a).self.runFree(unifier.down2(e)))
  //     })
  // }

  implicit class FreeVanFXSyntax[L <: HListK, A](val h: FreeVan[Effects[L, ?[_]], A])  {
    def expand[L2 <: HListK](implicit subList: SubList[L, L2]): FreeVanFX[L2, A] = 
      // FreeVanFX[L2, A](
        new FreeVan[Effects[L2, ?[_]], A] {
          def runFree[M[_] : Monad](effs: Effects[L2, M]): M[A] = h.runFree(subList(effs))
        }
      // )


    def flatMap[B, L2 <: HListK, L3 <: HListK, L4 <: HListK](f: A => FreeVanFX[L2, B])(
      implicit unifier: Unifier.Aux[Effects[L, ?[_]], Effects[L2, ?[_]], Effects[L3, ?[_]]]
    ): FreeVan[Effects[L3, ?[_]], B] =
      new FreeVan[Effects[L3, ?[_]], B] {
        def runFree[M[_] : Monad](e: Effects[L3, M]): M[B] = Monad[M].flatMap(h.runFree(unifier.down(e)))(a => f(a).runFree(unifier.down2(e)))
      }

  }

  implicit def isoK[L <: HListK, A, L2 <: HListK](h: FreeVan[Effects[L, ?[_]], A])(
    implicit isoK: IsoK[L, L2]
  ): FreeVan[Effects[L2, ?[_]], A] =
    new FreeVan[Effects[L2, ?[_]], A] {
          def runFree[M[_] : Monad](effs: Effects[L2, M]): M[A] = h.runFree(isoK.from(effs))
        }

  object FreeVanFX {

    // def pure[L <: HListK, A](x: A): FreeVanFX[L, A] = FreeVanFX[L, A](FreeVan.pure[Effects[L, ?[_]], A](x))

    def liftInterpreter0[E[_[_]], A](h: Interpreter[E, A]): FreeVanFX[E |: HNilK, A] =
      // FreeVanFX[E |: HNilK, A](
        new FreeVan[Effects[E |: HNilK, ?[_]], A] {
          def runFree[M[_] : Monad](effs: Effects[E |: HNilK, M]): M[A] = {
            val ConsFX(head, _) = effs
            h(head)
          }
        }
      // )

    def liftInterpreter[L <: HListK, E[_[_]], A](h: Interpreter[E, A])(implicit eCont: Contains[L, E]): FreeVanFX[L, A] =
      // FreeVanFX[L, A](
        new FreeVan[Effects[L, ?[_]], A] {
          def runFree[M[_] : Monad](effs: Effects[L, M]): M[A] = {
            val (e, _) = eCont.extract(effs)
            h(e)
          }
        }
      // )

    // def liftInterpreter2[L <: HListK, E[_[_], _]](h: Interpreter[E[?[_], ?], A])(implicit contains: Contains[L, E[?[_], ?]]): FreeVanFX[L, A] =
    //   new FreeVan[Effects[L, ?[_]], A] {
    //     def runFree[M[_] : Monad](effs: Effects[L, M]): M[A] = h(contains(effs))
    //   }
  }

  // implicit class InterpreterSyntax[E[_[_]], A](val h: Interpreter[E, A]) extends AnyVal {
  //   def liftVan[L <: HListK](implicit contains: Contains[L, E]): FreeVanFX[L, A] =  FreeVanFX.liftInterpreter(h)
  // }

  // implicit class InterpreterSyntax2[E[_[_], _], S, A](val h: Interpreter[E[?[_], S], A]) extends AnyVal {
  //   def liftVan[L <: HListK](implicit contains: Contains[L, E[?[_], S]]): FreeVanFX[L, A] =  FreeVanFX.liftInterpreter2(h)
  // }
  
  // implicit class InterpreterTSyntax[E[_[_]], A](val h: InterpreterT[E, A]) extends AnyVal {
  //   def liftVan[L <: HListK](implicit contains: Contains[L, E]): FreeVanFX[L, A] =  FreeVanFX.liftInterpreterT(h)
  // }

} 