import ops.DeepMerger
import ops.Depth
import ops.RecordSubType
import ops.Width
//import ops.WidthSubtype

package object syntax {
  import shapeless.HList

  implicit class RecordSyntax[R <: HList](val r: R) extends AnyVal{
    def deepMerge[R1 <: HList](r1: R1)(implicit dm: DeepMerger[R, R1]): dm.Out = dm(r, r1)
    def deepExtract[P <: HList](implicit dst: RecordSubType[R, P, Depth]) = dst(r)
    def extract[P <: HList](implicit wst: RecordSubType[R, P, Width]) = wst(r)
  }

}
