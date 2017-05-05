package ops

import shapeless.HList
import shapeless.HNil
import shapeless.labelled.{FieldType, field}
import shapeless.ops.record.Remover
import shapeless.::

trait ExtractionMode
trait Depth extends ExtractionMode
trait Width extends ExtractionMode


//TODO: use the same approach to reuse DepthMerger code in Merger typeclass

//extracts super-record from sub-record (witnesses depth/width subtyping relation)
trait Extractor[C <: HList, P <: HList, T] extends Function1[C, P]

trait LowExtractor {
  implicit def plain[C <: HList, K, V, V1, CT <: HList, PT <: HList, T <: ExtractionMode](
    implicit r: Remover.Aux[C, K, (V1, CT)],
    ev: V1 <:< V,
    ds: Extractor[CT, PT, T]
  ): Extractor[C, FieldType[K, V] :: PT, T] = new Extractor[C, FieldType[K, V] :: PT, T] {
    def apply(c: C): FieldType[K, V] :: PT = {
      val (h, t) = r(c)
      field[K](ev(h)) :: ds(t)
    }
  }
}

object Extractor extends LowExtractor {

  implicit def hnil[C <: HList, P <: HList, T <: ExtractionMode](implicit ev: HNil =:= P): Extractor[C, P, T] = new Extractor[C, P, T] {
    def apply(c: C): P = HNil
  }

  implicit def nested[C <: HList, K, V <: HList, V1 <: HList, CT <: HList, PT <: HList](
    implicit r: Remover.Aux[C, K, (V1, CT)],
    ds1: Extractor[V1, V, Depth],
    ds2: Extractor[CT, PT, Depth]
  ): Extractor[C, FieldType[K, V] :: PT, Depth] = new Extractor[C, FieldType[K, V] :: PT, Depth] {
    def apply(c: C): FieldType[K, V] :: PT = {
      val (h, t) = r(c)
      field[K](ds1(h)) :: ds2(t)
    }
  }

}