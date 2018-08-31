package clustering4ever.util

import scala.collection.{GenSeq, mutable, parallel}
import clustering4ever.scala.measurableclass.BinaryScalarVector

trait CommonTypes
{
	type MB[T] = mutable.Buffer[T]
	type PA[T] = parallel.mutable.ParArray[T]
	type BSV[Vb <: GenSeq[Int], Vs <: GenSeq[Double]] = BinaryScalarVector[Vb, Vs]
}