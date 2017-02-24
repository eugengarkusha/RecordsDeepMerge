import ops.DeepMerge

package object syntax {
  import shapeless.HList

  implicit class RecordSyntax[R <: HList](val r: R) extends AnyVal{
    def mergeInto[R1 <: HList](r1: R1)(implicit dm: DeepMerge[R, R1]): dm.Out = dm(r, r1)
  }

}
