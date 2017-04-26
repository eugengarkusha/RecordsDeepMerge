//import ops.DepthSubtypeOf
import org.scalatest.FunSuite
import org.scalatest.Matchers
import shapeless.record.Record
//import shapeless.test.illTyped
import syntax.RecordSyntax


class DeepMergeTest extends FunSuite with Matchers {

  test("deep merge") {

    val inner1 = Record(d = "D", e = false)
    val inner11 = Record(d = "z", e = true)
    val outer1 = Record(d = 10, e = inner1, x = "boo")
    val outer11 = Record(d = 11, e = inner11, x = "bar")
    val inner2 = Record(d = 3, m = 2D)
    val outer2 = Record(x = "foo", d = -1, e = inner2)

    //nested
    outer1.mergeInto(outer2) shouldBe Record(x = "boo", d = 10, e = Record(d = "D", m = 2D, e = false))
    outer2.mergeInto(outer1) shouldBe Record(d = -1, e = Record(d = 3, e = false, m = 2D), x = "foo")

    //complete intersection
    outer1.mergeInto(outer11) shouldBe outer1
    outer11.mergeInto(outer1) shouldBe outer11

    //plain
    val inner3Child = Record(x = "X1", m = "M")
    val inner3 = Record(d = inner3Child, e = true, x = "X")
    val inner4 = Record(d = "D", e = false, x = 2, m = 6)
    inner3.mergeInto(inner4) shouldBe Record(d = inner3Child, e = true, x = "X", m = 6)
    inner4.mergeInto(inner3) shouldBe Record(d = "D", e = false, x = 2,  m = 6)

  }



//  test("depth subtype") {
//
//    trait X
//    trait Y extends X
//
//    type Inner11 = Record.`'e->Boolean, 'm->Double, 'd->Y`.T
//    type Outer11 = Record.`'e->Inner11, 'd->Int, 'x -> String`.T
//    type Inner22 = Record.`'d->X, 'm->Double`.T
//    type Outer22 = Record.`'d->Int, 'e->Inner22`.T
//
//    implicitly[Outer11 DepthSubtypeOf Outer22]
//
//    illTyped("""implicitly[Outer22 DepthSubtypeOf Outer11]""")
//  }

}