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


trait State[S, M[_]] {
  def get(): M[S]
  def put(s: S): M[Unit]
}

object State {

  def get[S]: FreeVanFX[λ[M[_] => State[S, M]] |: HNilK, S] =
    FreeVanFX.liftInterpreter0[λ[M[_] => State[S, M]], S](
      new Interpreter[λ[M[_] => State[S, M]], S] { def apply[M[_]](st: State[S, M]): M[S] = st.get }
    )

  def put[S](s: S): FreeVanFX[λ[M[_] => State[S, M]] |: HNilK, Unit] =
    FreeVanFX.liftInterpreter0[λ[M[_] => State[S, M]], Unit](
      new Interpreter[λ[M[_] => State[S, M]], Unit] { def apply[M[_]](st: State[S, M]): M[Unit] = st.put(s) }
    )

}