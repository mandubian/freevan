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


class FreeVanSpec extends FlatSpec with Matchers with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))
  

  "FreeVan" should "run basic program" in {
    // Random Effect definition
    trait Random[M[_]] {
      def getRandEff: M[Int]
    }

    // lifting helpers to FreeVan
    object Random {
      def getRand[FX <: FXList](implicit hasFX: HasFX[FX, Random]): FreeVan[FX, Int] = (
        new ForAllM[Random, Int] { def apply[M[_]](e: Random[M]): M[Int] = e.getRandEff }
      ).liftVan[FX]
    }

    // Stdio Effect definition
    trait StdIO[M[_]] {
      def readLine: M[String]

      def putLine(a: String): M[Unit]
    }

    // lifting helpers to FreeVan
    object StdIO {
      def readLine[FX <: FXList](implicit hasFX: HasFX[FX, StdIO]): FreeVan[FX, String] = (
        new ForAllM[StdIO, String] { def apply[M[_]](e: StdIO[M]): M[String] = e.readLine }
      ).liftVan[FX]

      def putLine[FX <: FXList](a: String)(implicit hasFX: HasFX[FX, StdIO]): FreeVan[FX, Unit] = (
        new ForAllM[StdIO, Unit] { def apply[M[_]](e: StdIO[M]): M[Unit] = e.putLine(a)  }
      ).liftVan[FX]

    }

    // Define effect stack
    type FX = Random |: StdIO |: FXNil

    def program: FreeVan[FX, Unit] = for {
      _ <- StdIO.putLine[FX](s"Enter something:")
      i <- Random.getRand[FX]
      a <- StdIO.readLine[FX]
      _ <- StdIO.putLine[FX](s"read $a & got random $i\n")
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