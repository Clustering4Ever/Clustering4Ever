package clustering4ever.scala.measurableclass
/**
 * @author Beck Gaël
 */
/**
 * A wrapper for mixt vector
 */
abstract class BinaryScalarVector[Vb <: Seq[Int], Vs <: Seq[Double]](val binary: Vb, val scalar: Vs) extends Serializable
/**
 * An instanciation of a mixt vector
 */
case class SimpleBinaryScalarVector[Vb <: Seq[Int], Vs <: Seq[Double]](override val binary: Vb, override val scalar: Vs) extends BinaryScalarVector[Vb, Vs](binary, scalar)