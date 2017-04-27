package ops

import shapeless.HList
import shapeless.HNil
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record.Remover
import shapeless.::

trait SubtypingMode
trait Depth extends SubtypingMode
trait Width extends SubtypingMode


//TODO: use the same approach to reuse DepthMerger code in Merger typeclass
//Do we need symbolic type aliases for Depth/Witdth subtypes ( like <<: or smth)
trait RecordSubType[C <: HList, P <: HList, +T] extends Function1[C, P]

trait LLO {
  implicit def plain[C <: HList, K, V, V1, CT <: HList, PT <: HList, T <: SubtypingMode](
    implicit r: Remover.Aux[C, K, (V1, CT)],
    ev: V1 <:< V,
    ds: RecordSubType[CT, PT, T]
  ): RecordSubType[C, FieldType[K, V] :: PT, T] = new RecordSubType[C, FieldType[K, V] :: PT, T] {
    def apply(c: C): FieldType[K, V] :: PT = {
      val (h, t) = r(c)
      field[K](ev(h)) :: ds(t)
    }
  }
}

object RecordSubType extends LLO {

  implicit def hnil[C <: HList, P <: HList, T <: SubtypingMode](implicit ev: HNil =:= P): RecordSubType[C, P, T] = new RecordSubType[C, P, T] {
    def apply(c: C): P = HNil
  }

  implicit def nested[C <: HList, K, V <: HList, V1 <: HList, CT <: HList, PT <: HList](
    implicit r: Remover.Aux[C, K, (V1, CT)],
    ds1: RecordSubType[V1, V, Depth],
    ds2: RecordSubType[CT, PT, Depth]
  ): RecordSubType[C, FieldType[K, V] :: PT, Depth] = new RecordSubType[C, FieldType[K, V] :: PT, Depth] {
    def apply(c: C): FieldType[K, V] :: PT = {
      val (h, t) = r(c)
      field[K](ds1(h)) :: ds2(t)
    }
  }

}