package clustering4ever.scala.measurableclass

case class BinaryScalarVector[Vb <: Seq[Int], Vs <: Seq[Double]](val binary: Vb, val scalar: Vs) extends Serializable