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


// import scala.language.existentials
import scala.language.experimental.macros

// import scala.annotation.{ StaticAnnotation, tailrec }
// import scala.reflect.api.Universe
import scala.reflect.macros.{ whitebox }


object Expander {

  def apply[L <: HListK, A](body: Any): FreeVanFX[L, A] = macro FXMacros.mkFXImpl[L, A]

}

class FXMacros(val c: whitebox.Context) {
  import c.universe._

  def mkFXImpl[L <: HListK, A](body: c.Tree)(implicit lTag: WeakTypeTag[L], aTag: WeakTypeTag[A]): Tree = {
    body match {
      case q"for (..$enums) yield $r" =>
        // q"for
        val es = enums.map { case cc =>
          val fq"$k <- $v" = cc
          println(s"$k $v")
          fq"k <- ${v}.expand[$lTag]"
        }
       // val res = q"for (..$es) yield $r"
       // println(s"$res")
       q"for { i <- Random.getRand } yield ()"
        
      // case body => q"$body.expand[$lTag]"
    }
  }
}

