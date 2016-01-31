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


// Stdio Effect definition
trait StdIO[M[_]] {
  def readLine: M[String]

  def putLine(a: String): M[Unit]
}

// lifting helpers to FreeVanFX
object StdIO {
  val readLine: FreeVanFX[StdIO |: HNilK, String] = FreeVanFX.liftInterpreter0(
    new Interpreter[StdIO, String] { def apply[M[_]](e: StdIO[M]): M[String] = e.readLine }
  )

  def putLine[M[_]](a: String): FreeVanFX[StdIO |: HNilK, Unit] = FreeVanFX.liftInterpreter0(
    new Interpreter[StdIO, Unit] { def apply[M[_]](e: StdIO[M]): M[Unit] = e.putLine(a)  }
  )

  def defaultHandler[M[_] : Applicative]: StdIO[M] = new StdIO[M] {
    def readLine: M[String] = Applicative[M].pure(scala.io.StdIn.readLine())
    def putLine(a: String): M[Unit] = Applicative[M].pure(println(a))
  }

}


