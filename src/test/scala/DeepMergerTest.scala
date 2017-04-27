//import ops.DepthSubtypeOf
import org.scalatest.FunSuite
import org.scalatest.Matchers
import shapeless.record.Record
//import shapeless.test.illTyped
import syntax.RecordSyntax
import shapeless.record._


class DeepMergerTest extends FunSuite with Matchers {

  test("deep merge") {

    //"behaves identically to RecordOps.merge for non-nested records"
    val r3 = Record(d = Record(x = "X1", m = "M"), e = true, x = "X")
    val r4 = Record(d = "D", e = false, x = 2, m = 6)
    val r5 = Record(d = "A", d = "B", d  = "C")

    r4.merge(r3) shouldBe r4.deepMerge(r3)
    r3.merge(r4) shouldBe r3.deepMerge(r4)
    r3.merge(r5) shouldBe r3.deepMerge(r5)
    r5.merge(r3) shouldBe r5.deepMerge(r3)

    //nested
    val inner1 = Record(d = "D", e = false)
    val inner2 = Record(d = 3, m = 2D)
    val outer1 = Record(d = 10, e = inner1, x = "boo")
    val outer2 = Record(x = "foo", d = -1, e = inner2)

    val innerMerged12 = inner1.merge(inner2)
    val innerMerged21 = inner2.merge(inner1)
    outer1.deepMerge(outer2) shouldBe Record(d = -1, e = innerMerged12,  x = "foo")
    outer2.deepMerge(outer1) shouldBe Record(x = "boo", d = 10, e = innerMerged21)
//
    //complete intersection
    val inner11 = Record(d = "D2", e = true)
    val outer11 = Record(d = 11, e = inner11, x = "bar")
    outer1.deepMerge(outer11) shouldBe outer11
    outer11.deepMerge(outer1) shouldBe outer1
    type x1 = Record.`'d->String`.T
  type x= Record.`'d->Int, 'e->x1`.T
    println(outer1.extract[x])
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