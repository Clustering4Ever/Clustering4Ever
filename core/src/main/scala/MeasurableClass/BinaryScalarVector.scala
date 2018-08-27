package clustering4ever.scala.measurableclass

import scala.collection.immutable

case class BinaryScalarVector[Vb <: Seq[Int], Vs <: Seq[Double]](val binary: Vb, val scalar: Vs)