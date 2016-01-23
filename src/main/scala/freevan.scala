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


trait FreeVan[Ops[_[_]], A] {
  self =>

  def runFree[M[_] : Monad](e: Ops[M]): M[A]

  def map[B](f: A => B): FreeVan[Ops, B] =
    new FreeVan[Ops, B] {
      def runFree[M[_] : Monad](e: Ops[M]): M[B] = Monad[M].map(self.runFree(e))(f)
    }

  def flatMap[B](f: A => FreeVan[Ops, B]): FreeVan[Ops, B] =
    new FreeVan[Ops, B] {
      def runFree[M[_] : Monad](e: Ops[M]): M[B] = Monad[M].flatMap(self.runFree(e))(a => f(a).runFree(e))
    }

}


object FreeVan {

  implicit def monad[Ops[_[_]]] : Monad[FreeVan[Ops, ?]] = new Monad[FreeVan[Ops, ?]] {
    def pure[A](x: A): FreeVan[Ops, A] = new FreeVan[Ops, A] {
      def runFree[M[_] : Monad](e: Ops[M]): M[A] = Monad[M].pure(x)
    }

    def flatMap[A, B](fa: FreeVan[Ops, A])(f: A => FreeVan[Ops, B]): FreeVan[Ops, B] =
      new FreeVan[Ops, B] {
        def runFree[M[_] : Monad](e: Ops[M]): M[B] = Monad[M].flatMap(fa.runFree(e))(a => f(a).runFree(e))
      }
  }

}


