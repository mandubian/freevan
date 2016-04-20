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
// package freevan
// package effects


// trait UnapplyK[TCA, M[_]] {
//   type TC[_[_]]

//   def subst(tca: TCA): TC[M]
// }

// object UnapplyK extends LowerUnapplyK {

//   type Aux[TCA0, TC0[_[_]], M0[_]] = UnapplyK[TCA0, M0] { type TC[m[_]] = TC0[m] }

//   implicit def one[TC0[_[_]], M0[_]]: UnapplyK.Aux[TC0[M0], TC0, M0] = new UnapplyK[TC0[M0], M0] {
//     type TC[m[_]] = TC0[m]

//     def subst(tca: TC0[M0]): TC[M0] = tca
//   }

// }

// trait LowerUnapplyK {
//   implicit def two[TC0[_[_], _], M0[_], A]: UnapplyK.Aux[TC0[M0, A], TC0[?[_], A], M0] = new UnapplyK[TC0[M0, A], M0] {
//     type TC[m[_]] = TC0[m, A]

//     def subst(tca: TC0[M0, A]): TC[M0] = tca
//   }
// }
