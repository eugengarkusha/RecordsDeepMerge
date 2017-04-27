package ops

import shapeless.HList
import shapeless.HNil
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record.Remover
import shapeless.::
import shapeless.<:!<

//TODO: WidthSubtype
//Do we need symbolic type aliases for Depth/Witdth subtypes ( like <<: or smth)
trait DepthSubtype[C <: HList, P <: HList] extends Function1[C, P]

trait LLO {
  implicit def plain[C <: HList, K, V, V1, CT <: HList, PT <: HList](
    implicit r: Remover.Aux[C, K, (V1, CT)],
    ev: V1 <:< V,
    ds: DepthSubtype[CT, PT]
  ): DepthSubtype[C, FieldType[K, V] :: PT] = new DepthSubtype[C, FieldType[K, V] :: PT] {
    def apply(c: C): FieldType[K, V] :: PT = {
      val (h, t) = r(c)
      field[K](ev(h)) :: ds(t)
    }
  }
}

object DepthSubtype extends LLO{

  implicit def hnil[C <: HList, P <: HList](implicit ev: HNil =:= P): DepthSubtype[C, P] = new DepthSubtype[C, P] {
    def apply(c: C): P = HNil
  }

  implicit def deep[C <: HList, K, V <: HList, V1<:HList, CT <: HList, PT <: HList](
    implicit r: Remover.Aux[C, K, (V1, CT)],
    ds1: DepthSubtype[V1, V],
    ds2: DepthSubtype[CT, PT]
  ): DepthSubtype[C, FieldType[K, V] :: PT] = new DepthSubtype[C, FieldType[K, V] :: PT] {
    def apply(c: C): FieldType[K, V] :: PT = {
      val (h, t) = r(c)
      field[K](ds1(h)) :: ds2(t)
    }
  }

}