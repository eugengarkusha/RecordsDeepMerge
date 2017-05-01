import shapeless.HList

package object ops {
  type DeepExtractor[C <: HList, P <: HList] = Extractor[C, P, Depth]
  type PlainExtractor[C <: HList, P <: HList] = Extractor[C, P, Depth]
}
