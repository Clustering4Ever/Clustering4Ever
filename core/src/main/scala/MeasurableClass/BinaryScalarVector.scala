package clustering4ever.scala.measurableclass

import scala.collection.GenSeq

case class BinaryScalarVector[Vb <: GenSeq[Int], Vs <: GenSeq[Double]](val binary: Vb, val scalar: Vs) extends Serializable