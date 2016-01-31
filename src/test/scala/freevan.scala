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

import effects._


class FreeVanFXSpec extends FlatSpec with Matchers with ScalaFutures {

  implicit val defaultPatience =
    PatienceConfig(timeout =  TSpan(300, Seconds), interval = TSpan(5, Millis))
  
/*
  "FreeVanFX" should "run basic program" in {

    // Define effect stack
    type FX = Random |: State[?[_], String] |: StdIO |: HNilK

    implicitly[Contains.Aux[FX, State[?[_], String], Random |: StdIO |: HNilK]]

    // Append.headNotContained[HNilK, State[?[_], String], Random |: StdIO |: HNilK, Random |: StdIO |: HNilK]
    // implicitly[Append.Aux[State[?[_], String]|: HNilK, Random |: StdIO |: HNilK, State[?[_], String] |: Random |: StdIO |: HNilK]]

    def program: FreeVanFX[FX, Unit] = for {
      i <- Random.getRand.expand[FX]
      b <- State.get[String].expand[FX]
      _ <- State.put(b + i).expand[FX]
      c <- State.get[String].expand[FX]
      _ <- StdIO.putLine(s"state is $c\n").expand[FX]
    } yield ()

    // Define effect handlers
    val randomH = Random.defaultHandler[Future]

    val stdIOH = StdIO.defaultHandler[Future]

    def stateH(initial: String): State[Future, String] = new State[Future, String] {
      var i = initial
      def get(): Future[String] = Future.successful(i)
      def put(a: String): Future[Unit] = Future { i = a }
    }

    val effects = randomH :: stateH("a") :: stdIOH :: NoFX[Future]()
    val res = program.runFree(effects).futureValue
    println(s"Result: $res")

  }
*/
  "FreeVanFX" should "unify basic program" in {

    type FX = Random |: StdIO |: State[?[_], String] |: HNilK

    val program: FreeVanFX[FX, Unit] = for {
      i <- Random.getRand
      b <- State.get[String]
      _ <- State.put(b + i)
      c <- State.get[String]
      _ <- StdIO.putLine(s"final $c")
    } yield ()

    val randomH = Random.defaultHandler[Future]

    val stdIOH = StdIO.defaultHandler[Future]

    def stateH(initial: String): State[Future, String] = new State[Future, String] {
      var i = initial
      def get(): Future[String] = Future.successful(i)
      def put(a: String): Future[Unit] = Future { i = a }
    }

    val effects = randomH :: stdIOH :: stateH("a") :: NoFX[Future]()
    val res = program.runFree(effects).futureValue
    println(s"Result: $res")
  }

}



    // implicitly[
    //   Contains[State[?[_], String] |: HNilK, State[?[_], String]]
    // ]

    // implicitly[
    //   SubList[State[?[_], String] |: HNilK, State[?[_], String] |: HNilK]
    // ]

    // val a = implicitly[
    //   SubList[State[?[_], String] |: HNilK, FX]
    // ]

    // implicitly[
    //   SubList[State[?[_], String] |: HNilK, Random |: State[?[_], String] |: StdIO |: HNilK]
    // ]

    // implicitly[
    //   SubList[Random |: HNilK, Random |: State[?[_], String] |: StdIO |: HNilK]
    // ]
