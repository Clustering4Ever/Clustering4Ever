package clustering4ever.scala.measurableclass
/**
 * @author Beck Gaël
 */
/**
 * A wrapper for mixt vector
 */
case class BinaryScalarVector[Vb <: Seq[Int], Vs <: Seq[Double]](val binary: Vb, val scalar: Vs) extends Serializable