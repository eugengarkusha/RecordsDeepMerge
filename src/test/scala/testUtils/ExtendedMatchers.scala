package testUtils

import org.scalatest.Matchers

trait ExtendedMatchers extends Matchers {
  def cmp[T, T1](r1: T, r2: T1)(implicit ev: T <:< T1) = r1 shouldBe r1
}
