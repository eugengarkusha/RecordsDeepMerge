package ops

import shapeless.HList

//witnesses that R1 is the depth subtype of R2
trait DepthSubtypeOf [R1 <: HList, R2 <: HList]

object DepthSubtypeOf{
  implicit def inst [R1 <: HList, R2 <: HList, O <: HList](implicit ev: DeepMerge.Aux[R2, R1, O], ev1: O =:= R1): DepthSubtypeOf[R1, R2] = null
}
