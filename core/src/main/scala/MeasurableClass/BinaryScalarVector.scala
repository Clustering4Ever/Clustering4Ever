package clustering4ever.scala.measurableclass

import scala.collection.immutable

case class BinaryScalarVector[Vb <: immutable.Seq[Int], Vr <: immutable.Seq[Double]](val binary: Vb, val scalar: Vr)