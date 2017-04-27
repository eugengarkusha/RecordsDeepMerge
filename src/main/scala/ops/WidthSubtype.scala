//package ops
//
//import shapeless.::
//import shapeless.HList
//import shapeless.HNil
//import shapeless.labelled.FieldType
//import shapeless.labelled.field
//import shapeless.ops.record.Remover
//
//trait WidthSubtype[C <: HList, P <: HList] extends DepthSubtype[C, P]
//
//trait Impl {
//  implicit def hnil[C <: HList, P <: HList](implicit ev: HNil =:= P): WidthSubtype[C, P] = new WidthSubtype[C, P] {
//    def apply(c: C): P = HNil
//  }
//
//  implicit def plain[C <: HList, K, V, V1, CT <: HList, PT <: HList](
//    implicit r: Remover.Aux[C, K, (V1, CT)],
//    ev: V1 <:< V,
//    ds: WidthSubtype[CT, PT]
//  ): WidthSubtype[C, FieldType[K, V] :: PT] = new WidthSubtype[C, FieldType[K, V] :: PT] {
//    def apply(c: C): FieldType[K, V] :: PT = {
//      val (h, t) = r(c)
//      field[K](ev(h)) :: ds(t)
//    }
//  }
//}
//object WidthSubtype extends Impl
//
