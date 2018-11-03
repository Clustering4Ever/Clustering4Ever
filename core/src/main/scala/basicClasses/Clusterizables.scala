package clustering4ever.scala.clusterizables
/**
 * @author Beck Gaël
 */
import scala.collection.immutable
import spire.math.{Numeric => SNumeric}
import clustering4ever.scala.vectorizables.{Vectorizable, MixtVectorizable, RealVectorizable, BinaryVectorizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.scala.basicenum.AlternativeVectorNature._
/**
 *
 */
abstract class Clusterizable[@specialized(Int, Long) ID: Numeric, O, V, Self <: Clusterizable[ID, O, V, Self]](
	val id: ID, 
	val vectorizable: Vectorizable[O, V],
	val clusterID: Option[Int] = None,
	val altVectors: Option[immutable.HashMap[AlternativeVector, V]] = None
) extends Serializable {
	/**
	 *
	 */
	final lazy val vector: V = vectorizable.toVector
	/**
	 *
	 */
	def setAltVector(idx: AlternativeVector, altVector: V): Self
	/**
	 *
	 */
	def setClusterID(newCID: Int): Self
}
/**
 *
 */
abstract class RealClusterizable[ID: Numeric, O, V <: Seq[Double], Self <: RealClusterizable[ID, O, V, Self]](
	id: ID,
	vectorizable: RealVectorizable[O, V],
	clusterID: Option[Int] = None,
	altVectors: Option[immutable.HashMap[AlternativeVector, V]] = None
) extends Clusterizable[ID, O, V, Self](id, vectorizable, clusterID, altVectors)
/**
 *
 **/
case class SimpleRealClusterizable[ID: Numeric, O, V <: Seq[Double]](
	override val id: ID,
	override val vectorizable: RealVectorizable[O, V],
	override val clusterID: Option[Int] = None,
	override val altVectors: Option[immutable.HashMap[AlternativeVector, V]] = None
) extends RealClusterizable[ID, O, V, SimpleRealClusterizable[ID, O, V]](id, vectorizable, clusterID, altVectors) {
	/**
	 *
	 */
	override def canEqual(a: Any): Boolean = a.isInstanceOf[SimpleRealClusterizable[ID, O, V]]
	/**
	 *
	 */
	override def equals(that: Any): Boolean = {
		that match {
		  case that: SimpleRealClusterizable[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
	/**
	 *
	 */
	def setAltVector(idx: AlternativeVector, altVector: V): SimpleRealClusterizable[ID, O, V] = this.copy(altVectors = Some(if( altVectors.isDefined ) altVectors.get + ((idx, altVector)) else immutable.HashMap(idx -> altVector) ))
	/**
	 *
	 */
	def setClusterID(newCID: Int): SimpleRealClusterizable[ID, O, V] = this.copy(clusterID = Some(newCID))
}
/**
 *
 */
abstract class BinaryClusterizable[ID: Numeric, O, V <: Seq[Int], Self <: BinaryClusterizable[ID, O, V, Self]](
	id: ID,
	vectorizable: BinaryVectorizable[O, V],
	clusterID: Option[Int] = None,
	altVectors: Option[immutable.HashMap[AlternativeVector, V]] = None
) extends Clusterizable[ID, O, V, Self](id, vectorizable, clusterID, altVectors)
/**
 *
 */
case class SimpleBinaryClusterizable[ID: Numeric, O, V <: Seq[Int]](
	override val id: ID,
	override val vectorizable: BinaryVectorizable[O, V],
	override val altVectors: Option[immutable.HashMap[AlternativeVector, V]] = None,
	override val clusterID: Option[Int] = None
) extends BinaryClusterizable[ID, O, V, SimpleBinaryClusterizable[ID, O, V]](id, vectorizable, clusterID, altVectors) {
	/**
	 *
	 */
	def canEqual(a: Any): Boolean = a.isInstanceOf[SimpleBinaryClusterizable[ID, O, V]]
	/**
	 *
	 */
	override def equals(that: Any): Boolean = {
		that match {
		  case that: SimpleBinaryClusterizable[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
	/**
	 *
	 */
	def setAltVector(idx: AlternativeVector, altVector: V): SimpleBinaryClusterizable[ID, O, V] = this.copy(altVectors = Some(if( altVectors.isDefined ) altVectors.get + ((idx, altVector)) else immutable.HashMap(idx -> altVector) ))
	/**
	 *
	 */
	def setClusterID(newCID: Int): SimpleBinaryClusterizable[ID, O, V] = this.copy(clusterID = Some(newCID))
}
/**
 *
 */
abstract class MixtClusterizable[
	ID: Numeric,
	O,
	Vb <: Seq[Int],
	Vs <: Seq[Double],
	Self <: MixtClusterizable[ID, O, Vb, Vs, Self]
](
	id: ID,
	vectorizable: MixtVectorizable[O, Vb, Vs],
	clusterID: Option[Int] = None,
	altVectors: Option[immutable.HashMap[AlternativeVector, BinaryScalarVector[Vb, Vs]]] = None
) extends Clusterizable[ID, O, BinaryScalarVector[Vb, Vs], Self](id, vectorizable, clusterID, altVectors)
/**
 * Basic clusterizable for Mixt Vectors => (V[Int], V[Double]) 
 */
case class SimpleMixtClusterizable[ID: Numeric, O, Vb <: Seq[Int], Vs <: Seq[Double]](
	override val id: ID,
	override val vectorizable: MixtVectorizable[O, Vb, Vs],
	override val clusterID: Option[Int] = None,
	override val altVectors: Option[immutable.HashMap[AlternativeVector, BinaryScalarVector[Vb, Vs]]] = None
) extends MixtClusterizable[ID, O, Vb, Vs, SimpleMixtClusterizable[ID, O, Vb, Vs]](id, vectorizable, clusterID, altVectors) {
	/**
	 *
	 */
	def canEqual(a: Any): Boolean = a.isInstanceOf[SimpleMixtClusterizable[ID, O, Vb, Vs]]
	/**
	 *
	 */
	override def equals(that: Any): Boolean = {
		that match {
		  case that: SimpleMixtClusterizable[ID, O, Vb, Vs] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
	/**
	 *
	 */
	def setAltVector(idx: AlternativeVector, altVector: BinaryScalarVector[Vb, Vs]): SimpleMixtClusterizable[ID, O, Vb, Vs] = this.copy(altVectors = Some(if( altVectors.isDefined ) altVectors.get + ((idx, altVector)) else immutable.HashMap(idx -> altVector) ))
	/**
	 *
	 */
	def setClusterID(newCID: Int): SimpleMixtClusterizable[ID, O, Vb, Vs] = this.copy(clusterID = Some(newCID))
}