import org.scalatest.FunSuite
import shapeless.record.Record
import testUtils.ExtendedMatchers
import syntax.RecordSyntax
import shapeless.record._


class DeepMergerTest extends FunSuite with ExtendedMatchers {

  test("deep merge") {

    //"behaves identically to RecordOps.merge for non-nested records"
    val r3 = Record(d = Record(x = "X1", m = "M"), e = true, x = "X")
    val r4 = Record(d = "D", e = false, x = 2, m = 6)
    val r5 = Record(d = "A", d = "B", d  = "C")

    cmp(r4.merge(r3) , r4.deepMerge(r3))
    cmp(r3.merge(r4) , r3.deepMerge(r4))
    cmp(r3.merge(r5) , r3.deepMerge(r5))
    cmp(r5.merge(r3) , r5.deepMerge(r3))


    //nested
    val inner1 = Record(d = "D", e = false)
    val inner2 = Record(d = 3, m = 2D)
    val outer1 = Record(d = 10, e = inner1, x = "boo")
    val outer2 = Record(x = "foo", d = -1, e = inner2)

    val innerMerged12 = inner1.merge(inner2)
    val innerMerged21 = inner2.merge(inner1)
    cmp(outer1.deepMerge(outer2) , Record(d = -1, e = innerMerged12,  x = "foo"))
    cmp(outer2.deepMerge(outer1) , Record(x = "boo", d = 10, e = innerMerged21))

    //complete intersection
    val inner11 = Record(d = "D2", e = true)
    val outer11 = Record(d = 11, e = inner11, x = "bar")
    cmp(outer1.deepMerge(outer11) , outer11)
    cmp(outer11.deepMerge(outer1) , outer1)

  }


}