import ops.DeepMerger

package object syntax {
  import shapeless.HList

  implicit class RecordSyntax[R <: HList](val r: R) extends AnyVal{
    def deepMerge[R1 <: HList](r1: R1)(implicit dm: DeepMerger[R, R1]): dm.Out = dm(r, r1)
  }

}
