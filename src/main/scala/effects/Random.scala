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

import cats.Applicative


trait Random[M[_]] {
  def getRand: M[Int]
}

// lifting helpers to FreeVanFX
object Random {
  val getRand: FreeVanFX[Random |: HNilK, Int] = FreeVanFX.liftInterpreter0(
    new Interpreter[Random, Int] { def apply[M[_]](e: Random[M]): M[Int] = e.getRand }
  )

  def defaultHandler[M[_] : Applicative]: Random[M] = new Random[M] {
    def getRand: M[Int] = Applicative[M].pure(scala.util.Random.nextInt())
  }
}

