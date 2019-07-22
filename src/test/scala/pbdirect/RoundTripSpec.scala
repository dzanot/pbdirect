/*
 * Copyright (c) 2019 Beyond the lines
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package pbdirect

import org.scalacheck.{Arbitrary, Gen, Properties, ScalacheckShapeless}
import org.scalacheck.Prop.{forAll, BooleanOperators}
import shapeless.{:+:, CNil}
import ScalacheckShapeless._

class RoundTripSpec extends Properties("Serialization") {
  type BIS = Boolean :+: Int :+: String :+: CNil
  case object Enum extends Enumeration {
    val First, Second = Value
  }
  case class IntVal(i: Int)
  property("simple int round trip") = forAll { o: IntVal =>
    val r = o.toPB.pbTo[IntVal]
    ("original = " + o) |: ("round tripped = " + r) |: (o == r)
  }

  case class StringVal(i: String)
  property("simple string round trip") = forAll { o: StringVal =>
    val r = o.toPB.pbTo[StringVal]
    ("original = " + o) |: ("round tripped = " + r) |: (o == r)
  }

  case class OptionThing(os: Option[String], i: Option[Int], b: Option[Boolean])
  property("option round trip") = forAll { o: OptionThing =>
    val r = o.toPB.pbTo[OptionThing]
    ("original = " + o) |: ("round tripped = " + r) |: (o == r)
  }

//  sealed trait Pet
//  case class Dog(s: String)
//  case class Cat(i: Int)
//
//  case class SealedThing(p: Pet)
//
//  implicit val catThings = Arbitrary.arbitrary[Dog]
//  implicit val dogThings = Arbitrary.arbitrary[Cat]
//
//  def genPet: Gen[Pet] = Gen.oneOf()
//
//  property("sealed trait round trip") = forAll { o: SealedThing =>
//    val r = o.toPB.pbTo[SealedThing]
//    ("original = " + o) |: ("round tripped = " + r) |: (o == r)
//  }

  case class CoproductThing(li: List[IntVal], b: Boolean, s: String, bis: BIS)
  implicit val coproductThings = for {
    ivs <- Arbitrary.arbitrary[List[Int]].map(_.map(IntVal))
    b   <- Arbitrary.arbitrary[Boolean]
    s   <- Arbitrary.arbitrary[String]
    bis <- Arbitrary.arbitrary[BIS]
  } yield CoproductThing(ivs, b, s, bis)

  property("coproduct round trip") = forAll { o: CoproductThing =>
    val r = o.toPB.pbTo[CoproductThing]
    ("original = " + o) |: ("round tripped = " + r) |: (o == r)
  }

  case class Outer(f: Float, msi: Map[String, Int], i: Int, inner: CoproductThing)
  implicit val outers = for {
    f     <- Arbitrary.arbitrary[Float]
    msi   <- Arbitrary.arbitrary[Map[String, Int]]
    i     <- Arbitrary.arbitrary[Int]
    inner <- coproductThings
  } yield Outer(f, msi, i, inner)

  property("complex round trip") = forAll { o: Outer =>
    val r = o.toPB.pbTo[Outer]
    ("original = " + o) |: ("round tripped = " + r) |: (o == r)
  }
}
