package ops


import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.labelled.FieldType
import shapeless.ops.record.Modifier
import shapeless.ops.record.Remover
import shapeless.labelled.field


trait DeepMerger[R1 <: HList, R2 <: HList] {
  type Out <: HList
  def apply(r1: R1, r2: R2): Out
}

trait Lo2 { self: DeepMerger.type =>

  implicit def add[H, T <: HList, R <: HList, MO <: HList](
    implicit m2: DeepMerger.Aux[T, R, MO]
  ): Aux[H :: T, R, H :: MO] = new DeepMerger[H :: T, R] {
    type Out = H :: MO
    def apply(r1: H :: T, r2: R): Out =  r1.head :: m2(r1.tail, r2)
  }
}
trait Lo1 extends Lo2 {self: DeepMerger.type =>

  implicit def replace[K, V , T <: HList, R <: HList, RT<:HList, MO1 <: HList, MO <: HList](
    implicit rm: Remover.Aux[R, K, (V, RT)],
    m: DeepMerger.Aux[T, RT, MO]
  ): Aux[FieldType[K, V] :: T, R, FieldType[K, V] :: MO] = new DeepMerger[FieldType[K, V] :: T, R] {
    type Out = FieldType[K, V] :: MO
    def apply(r1: FieldType[K, V] :: T, r2: R): Out = {
      val(rh, rt) = rm(r2)
      field[K](rh) :: m(r1.tail, rt)
    }
  }
}


object DeepMerger extends Lo1
{
  type Aux[R1 <: HList, R2 <: HList, O <: HList] = DeepMerger[R1, R2] {type Out = O}

  implicit def hnil[T <: HList]: Aux[HNil, T, T] = new DeepMerger[HNil, T] {
    type Out = T
    def apply(r1: HNil, r2: T): Out = r2
  }

  implicit def merge[K, V <: HList, T <: HList, R <: HList, V1 <: HList, RT <: HList,  MO1 <: HList, MO2 <: HList](
    implicit rm: Remover.Aux[R, K, (V1, RT)],
    m1: DeepMerger.Aux[V, V1, MO1],
    m2: DeepMerger.Aux[T, RT, MO2]
  ): Aux[FieldType[K, V] :: T, R, MO1 :: MO2] = new DeepMerger[FieldType[K, V] :: T, R] {
    type Out = MO1 :: MO2
    def apply(r1: FieldType[K, V] :: T, r2: R ): Out = {
      val (rh, rt) = rm(r2)
      m1(r1.head, rh) :: m2(r1.tail, rt)
    }
  }

}
