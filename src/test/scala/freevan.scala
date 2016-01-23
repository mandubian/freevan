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

import scala.concurrent.Future

import org.scalatest.concurrent.{AsyncAssertions, PatienceConfiguration, ScalaFutures}
import org.scalatest.{Assertions, BeforeAndAfterAll, FeatureSpec, FlatSpec, Matchers, Suite}
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.time.{Millis, Seconds, Span => TSpan}

import cats.std.future._

import scala.concurrent.ExecutionContext.Implicits.global


class FreeVanFXSpec extends FlatSpec with Matchers with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))
  

  "FreeVanFX" should "run basic program" in {
    // Random Effect definition
    trait Random[M[_]] {
      def getRandEff: M[Int]
    }

    // lifting helpers to FreeVanFX
    object Random {
      val getRand: FreeVanFX[Random |: HNilK, Int] = (
        new Interpreter[Random, Int] { def apply[M[_]](e: Random[M]): M[Int] = e.getRandEff }
      ).liftVan
    }

    // Stdio Effect definition
    trait StdIO[M[_]] {
      def readLine: M[String]

      def putLine(a: String): M[Unit]
    }

    // lifting helpers to FreeVanFX
    object StdIO {
      val readLine: FreeVanFX[StdIO |: HNilK, String] = (
        new Interpreter[StdIO, String] { def apply[M[_]](e: StdIO[M]): M[String] = e.readLine }
      ).liftVan

      def putLine[M[_]](a: String): FreeVanFX[StdIO |: HNilK, Unit] = (
        new Interpreter[StdIO, Unit] { def apply[M[_]](e: StdIO[M]): M[Unit] = e.putLine(a)  }
      ).liftVan

    }

    // Define effect stack
    type FX = Random |: StdIO |: HNilK

    def program: FreeVanFX[FX, Unit] = for {
      _ <- StdIO.putLine(s"Enter something:").expand[FX]
      i <- Random.getRand.expand[FX]
      a <- StdIO.readLine.expand[FX]
      _ <- StdIO.putLine(s"read $a & got random $i\n").expand[FX]
    } yield ()

    // Define effect handlers
    val theRandom = new Random[Future] {
      def getRandEff: Future[Int] = Future(scala.util.Random.nextInt())
    }

    val theStdIO = new StdIO[Future] {
      def readLine: Future[String] = Future(scala.io.StdIn.readLine())
      def putLine(a: String): Future[Unit] = Future(print(a))
    }

    val effects = theRandom :: theStdIO :: NoFX[Future]()
    val res = program.runFree(effects).futureValue
    println(s"Result: $res")

  }

}