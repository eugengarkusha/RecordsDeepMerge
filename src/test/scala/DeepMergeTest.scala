import ops.DepthSubtypeOf
import org.scalatest.FunSuite
import org.scalatest.Matchers
import shapeless.record.Record
import shapeless.test.illTyped
import syntax.RecordSyntax
class DeepMergeTest extends FunSuite with Matchers{


test("deep merge"){
  type Inner1 = Record.`'d->Int, 'e->Boolean`.T
  type Inner2 = Record.`'d->Int, 'm->Double`.T
  type Outer1 = Record.`'d->Int, 'e->Inner1`.T
  type Outer2 = Record.`'x -> String, 'd->Int, 'e->Inner2`.T


  val inner1: Inner1 = Record(d = 4, e = false)
  val inner2: Inner2 = Record(d = 3, m = 2D)
  val outer1: Outer1 = Record(d = 1, e = inner1)
  val outer2: Outer2 = Record(x = "foo", d = -1, e = inner2)

  outer1.mergeInto(outer2) shouldBe (Record(x = "foo", d = 1, e = Record(d = 4, m = 2D, e = false)))
  outer2.mergeInto(outer1) shouldBe (Record(d = -1, e = Record(d = 3, e = false, m = 2D), x = "foo"))

}

test("depth subtype"){
  type Inner11 = Record.`'e->Boolean, 'm->Double, 'd->Int`.T
  type Inner22 = Record.`'d->Int, 'm->Double`.T
  type Outer11 = Record.`'e->Inner11, 'd->Int, 'x -> String`.T
  type Outer22 = Record.`'d->Int, 'e->Inner22`.T

  implicitly[Outer11 DepthSubtypeOf Outer22]

  illTyped("""implicitly[Outer22 DepthSubtypeOf Outer11]""")
}


}
