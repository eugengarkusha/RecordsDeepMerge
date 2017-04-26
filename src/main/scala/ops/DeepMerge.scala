package ops

import ops.FieldRun.Aux
import shapeless.::
import shapeless.<:!<
import shapeless.HList
import shapeless.HNil
import shapeless.labelled.FieldType
import shapeless.labelled.field

trait DeepMerge[R1 <: HList, R2 <: HList] {
  type Out <: HList
  def apply(r1: R1, r2: R2): Out
}

object DeepMerge {
  type Aux[R1 <: HList, R2 <: HList, O <: HList] = DeepMerge[R1, R2] {type Out = O}

  implicit def hnil[T <: HList]: Aux[HNil, T, T] = new DeepMerge[HNil, T] {
    type Out = T
    def apply(r1: HNil, r2: T): Out = r2
  }

  implicit def recurse[K, V, T1 <: HList, R2 <: HList, O <: HList](
    implicit fr: FieldRun.Aux[K, V, R2, O],
    m: DeepMerge[T1, O]
  ): Aux[FieldType[K, V] :: T1, R2, m.Out] = new DeepMerge[FieldType[K, V] :: T1, R2] {
    type Out = m.Out
    def apply(r1: FieldType[K, V] :: T1, r2: R2): Out = m(r1.tail, fr.apply(r1.head, r2))

  }
}

private trait FieldRun[K, V, R2 <: HList] {
  type Out <: HList
  def apply(v: V, r2: R2): Out
}

private trait loFieldRun {
  implicit def replace[K, V, V1, R2T <: HList]
  : Aux[K, V, FieldType[K, V1] :: R2T, FieldType[K, V] :: R2T] = new FieldRun[K, V, FieldType[K, V1] :: R2T] {
    type Out = FieldType[K, V] :: R2T
    def apply(v: V, r2: FieldType[K, V1] :: R2T): Out = field[K](v) :: r2.tail
  }
}

private object FieldRun extends loFieldRun {
  type Aux[K, V, R2 <: HList, O <: HList] = FieldRun[K, V, R2] {type Out = O}

  implicit def merge[K, V <: HList, V2 <: HList, R2T <: HList](
    implicit m: DeepMerge[V, V2]
  ): Aux[K, V, FieldType[K, V2] :: R2T, FieldType[K, m.Out] :: R2T] = new FieldRun[K, V, FieldType[K, V2] :: R2T] {
    type Out = FieldType[K, m.Out] :: R2T
    def apply(v: V, r2: FieldType[K, V2] :: R2T): Out = field[K](m(v, r2.head)) :: r2.tail
  }

  implicit def add[K, V]: Aux[K, V, HNil, FieldType[K, V] :: HNil] = new FieldRun[K, V, HNil] {
    type Out = FieldType[K, V] :: HNil
    def apply(v: V, r2: HNil): Out = field[K](v) :: r2
  }

  implicit def miss[K, V, K2, V2, R2T <: HList](
    implicit ev: K <:!< K2,
    r: FieldRun[K, V, R2T]
  ): Aux[K, V, FieldType[K2, V2] :: R2T, FieldType[K2, V2] :: r.Out] = new FieldRun[K, V, FieldType[K2, V2] :: R2T] {
    type Out = FieldType[K2, V2] :: r.Out
    def apply(v: V, r2: FieldType[K2, V2] :: R2T): Out = r2.head :: r(v, r2.tail)
  }


}


