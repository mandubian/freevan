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
package object freevan {

  import cats.Monad

  implicit class InterpreterSyntax[E[_[_]], A](val h: Interpreter[E, A]) extends AnyVal {
    def liftVan[L <: HListK](implicit contains: Contains[L, E]): FreeVanFX[L, A] =  FreeVanFX.liftInterpreter(h)
  }

  type FreeVanFX[L <: HListK, A] = FreeVan[Effects[L, ?[_]], A]

  implicit class FreeVanFXSyntax[L <: HListK, A](val h: FreeVanFX[L, A]) extends AnyVal {
    def expand[L2 <: HListK](implicit subList: SubList[L, L2]): FreeVanFX[L2, A] = 
      new FreeVan[Effects[L2, ?[_]], A] {
        def runFree[M[_] : Monad](effs: Effects[L2, M]): M[A] = h.runFree(subList(effs))
      }
  }

  object FreeVanFX {
    def liftInterpreter[L <: HListK, E[_[_]], A](h: Interpreter[E, A])(implicit contains: Contains[L, E]): FreeVanFX[L, A] =
      new FreeVan[Effects[L, ?[_]], A] {
        def runFree[M[_] : Monad](effs: Effects[L, M]): M[A] = h(contains(effs))
      }
  }

}

