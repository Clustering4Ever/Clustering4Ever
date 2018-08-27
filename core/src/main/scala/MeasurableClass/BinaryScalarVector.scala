package clustering4ever.scala.measurableclass

import scala.collection.immutable

class BinaryScalarVector[VB <: immutable.Seq[Int], VR <: immutable.Seq[Double]](val binary: immutable.Seq[Int], val scalar: immutable.Seq[Double])