import org.scalatest.FunSuite
import shapeless.record._
import syntax._
import shapeless.test._
import testUtils.ExtendedMatchers

class ExtractorTest extends FunSuite with ExtendedMatchers {
  test("extract"){
    val inner1 = Record(d = 3, m = 2D, x= "X")
    val outer1 = Record(x = "foo", d = -1, e = inner1)

    cmp(outer1.extract[Record.`'x->String, 'd-> Int`.T], Record(x = "foo", d = -1))

    outer1.extract[Record.`'x->Any, 'd-> Any`.T] shouldBe Record(x = "foo", d = -1)

    type i = Record.`'x -> String, 'd->Int`.T
    cmp(outer1.deepExtract[Record.`'e->i, 'd-> Int`.T], Record(e = Record(x= "X", d = 3), d = -1))

    type ill1 = Record.`'d-> Int, 'z->Int`.T
    type ill2 = Record.`'x->i`.T
    type illIner = Record.`'m -> String, 'd->Int`.T
    type ill3 = Record.`'e->illIner, 'd-> Int`.T



    illTyped("outer1.extract[ill1]")
    illTyped("outer1.deepExtract[ill2]")
    illTyped("outer1.deepExtract[ill3]")
  }


}