package clustering4ever.scala.measurableclass
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import spire.math.{Numeric => SNumeric}
import scala.collection.mutable
import clustering4ever.util.SumVectors
/**
 * A wrapper for mixt vector
 */
case class BinaryScalarVector[Vb <: Seq[Int], Vs <: Seq[Double]](val binary: Vb, val scalar: Vs) extends Serializable