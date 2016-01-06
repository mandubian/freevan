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

import shapeless._

// trait FreeVan[Effect[_[_]], A]{
//   def runFree[M[_] : Monad](e: Effects[M]): M[A]
// }

// object FreeVan {

//   implicit def monad[Effect[_[_]]] : Monad[FreeVan[Effect, ?]] = new Monad[FreeVan[Effect, ?]] {
//     def pure[A](x: A): FreeVan[Effect, A] = new FreeVan[Effect, A] {
//       def runFree[M[_] : Monad](e: Effect[M]): M[A] = Monad[M].pure(x)
//     }

//     def flatMap[A, B](fa: FreeVan[Effect, A])(f: A => FreeVan[Effect, B]): FreeVan[Effect, B] =
//       new FreeVan[Effect, B] {
//         def runFree[M[_] : Monad](e: Effect[M]): M[B] = Monad[M].flatMap(fa.runFree(e))(a => f(a).runFree(e))
//       }
//   }

// }

trait FreeVan[FX <: FXList, A] {
  self =>
  def runFree[M[_] : Monad](e: Effects[FX, M]): M[A]

  def map[B](f: A => B): FreeVan[FX, B] = new FreeVan[FX, B] {
    def runFree[M[_] : Monad](e: Effects[FX, M]): M[B] = Monad[M].map(self.runFree(e))(f)
  }

  def flatMap[B](f: A => FreeVan[FX, B]): FreeVan[FX, B] =
    new FreeVan[FX, B] {
      def runFree[M[_] : Monad](e: Effects[FX, M]): M[B] = Monad[M].flatMap(self.runFree(e))(a => f(a).runFree(e))
    }
}


object FreeVan {

  implicit def monad[FX <: FXList] : Monad[FreeVan[FX, ?]] = new Monad[FreeVan[FX, ?]] {
    def pure[A](x: A): FreeVan[FX, A] = new FreeVan[FX, A] {
      def runFree[M[_] : Monad](e: Effects[FX, M]): M[A] = Monad[M].pure(x)
    }

    def flatMap[A, B](fa: FreeVan[FX, A])(f: A => FreeVan[FX, B]): FreeVan[FX, B] =
      new FreeVan[FX, B] {
        def runFree[M[_] : Monad](e: Effects[FX, M]): M[B] = Monad[M].flatMap(fa.runFree(e))(a => f(a).runFree(e))
      }
  }

  def liftM[FX <: FXList, E[_[_]], A](h: ForAllM[E, A])(implicit hasFX: HasFX[FX, E]): FreeVan[FX, A] = new FreeVan[FX, A] {
    def runFree[M[_] : Monad](effs: Effects[FX, M]): M[A] = h(hasFX(effs))
  }
}


