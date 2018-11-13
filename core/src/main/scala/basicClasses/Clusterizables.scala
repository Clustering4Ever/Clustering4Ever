package clustering4ever.scala.clusterizables
/**
 * @author Beck Gaël
 */
import scala.collection.immutable
import spire.math.{Numeric => SNumeric}
import clustering4ever.scala.vectorizables.{Vectorizable, Vector, MixtVectorizable, RealVectorizable, BinaryVectorizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.scala.basicenum.AlternativeVectorNature._
import clustering4ever.scala.basicenum.AlternativeVector
/**
 *
 */
trait ComplementaryVectors[V, Self <: ComplementaryVectors[V, Self]] {
	/**
	 *
	 */
	def setAltVector(idx: AlternativeVector, altVector: V): Self
	/**
	 *
	 */
	def setAltBinaryVector[Vb <: Seq[Int]](idx: AlternativeVector, altVector: Vb): Self
	/**
	 *
	 */
	def setAltScalarVector[Vs <: Seq[Double]](idx: AlternativeVector, altVector: Vs): Self
	/**
	 *
	 */
	val altVectors: Option[immutable.HashMap[AlternativeVector, V]]
	/**
	 *
	 */
	val altBinaryVectors: Option[immutable.HashMap[AlternativeVector, Seq[Int]]]
	/**
	 *
	 */
	val altScalarVectors: Option[immutable.HashMap[AlternativeVector, Seq[Double]]]
}
/**
 * Identified Object
 */
abstract class IdentifiedVectorizable[@specialized(Int, Long) ID: Numeric, O, V](
	val id: ID,
	val vectorizable: Vectorizable[O, V]
) extends Serializable {
	/**
	 *
	 */
	final lazy val vector: V = vectorizable.toVector

}
/**
 *
 */
case class IdentifiedVector[ID: Numeric, V](
	override val id: ID,
	override val vectorizable: Vector[V],
	override val altVectors: Option[immutable.HashMap[AlternativeVector, V]] = None,
	override val altBinaryVectors: Option[immutable.HashMap[AlternativeVector, Seq[Int]]] = None,
	override val altScalarVectors: Option[immutable.HashMap[AlternativeVector, Seq[Double]]] = None
) extends IdentifiedVectorizable[ID, V, V](id, vectorizable) with ComplementaryVectors[V, IdentifiedVector[ID, V]] {
	/**
	 *
	 */
	def setAltVector(idx: AlternativeVector, altVector: V) = this.copy(altVectors = Some(if( altVectors.isDefined ) altVectors.get + ((idx, altVector)) else immutable.HashMap(idx -> altVector) ))
	/**
	 *
	 */
	def setAltBinaryVector[Vb <: Seq[Int]](idx: AlternativeVector, altVector: Vb) = this.copy(altBinaryVectors = Some(if( altVectors.isDefined ) altBinaryVectors.get + ((idx, altVector)) else immutable.HashMap(idx -> altVector) ))
	/**
	 *
	 */
	def setAltScalarVector[Vs <: Seq[Double]](idx: AlternativeVector, altVector: Vs) = this.copy(altScalarVectors = Some(if( altVectors.isDefined ) altScalarVectors.get + ((idx, altVector)) else immutable.HashMap(idx -> altVector) ))
}
/**
 *
 */
abstract class Clusterizable[ID: Numeric, O, V, Self <: Clusterizable[ID, O, V, Self]](
	id: ID, 
	vectorizable: Vectorizable[O, V],
	val clusterID: Option[Int] = None
) extends IdentifiedVectorizable[ID, O, V](id, vectorizable) {
	/**
	 *
	 */
	def setClusterID(newCID: Int): Self
	/**
	 *
	 */
	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + id.hashCode
		result = prime * result + vector.hashCode
		result = prime * result + clusterID.hashCode
		result
	}

}
/**
 *
 */
abstract class ClusterizableExt[ID: Numeric, O, V, Self <: ClusterizableExt[ID, O, V, Self]](
	id: ID, 
	vectorizable: Vectorizable[O, V],
	clusterID: Option[Int] = None,
	val altVectors: Option[immutable.HashMap[AlternativeVector, V]] = None,
	val altBinaryVectors: Option[immutable.HashMap[AlternativeVector, Seq[Int]]] = None,
	val altScalarVectors: Option[immutable.HashMap[AlternativeVector, Seq[Double]]] = None
) extends Clusterizable[ID, O, V, Self](id, vectorizable, clusterID) with ComplementaryVectors[V, Self] {
	/**
	 *
	 */
	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + id.hashCode
		result = prime * result + vector.hashCode
		result = prime * result + clusterID.hashCode
		result = prime * result + altVectors.hashCode
		result = prime * result + altBinaryVectors.hashCode
		result = prime * result + altScalarVectors.hashCode
		result
	}

}
/**
 *
 **/
case class EasyClusterizable[ID: Numeric, O, V](
	override val id: ID,
	override val vectorizable: Vectorizable[O, V],
	override val clusterID: Option[Int] = None,
	override val altVectors: Option[immutable.HashMap[AlternativeVector, V]] = None,
	override val altBinaryVectors: Option[immutable.HashMap[AlternativeVector, Seq[Int]]] = None,
	override val altScalarVectors: Option[immutable.HashMap[AlternativeVector, Seq[Double]]] = None
) extends ClusterizableExt[ID, O, V, EasyClusterizable[ID, O, V]](id, vectorizable, clusterID, altVectors, altBinaryVectors, altScalarVectors) {
	/**
	 *
	 */
	override def canEqual(a: Any): Boolean = a.isInstanceOf[EasyClusterizable[ID, O, V]]
	/**
	 *
	 */
	override def equals(that: Any): Boolean = {
		that match {
		  case that: EasyClusterizable[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
	/**
	 *
	 */
	def setClusterID(newCID: Int): EasyClusterizable[ID, O, V] = this.copy(clusterID = Some(newCID))
	/**
	 *
	 */
	def setAltVector(idx: AlternativeVector, altVector: V): EasyClusterizable[ID, O, V] = this.copy(altVectors = Some(if( altVectors.isDefined ) altVectors.get + ((idx, altVector)) else immutable.HashMap(idx -> altVector) ))
	/**
	 *
	 */
	def setAltBinaryVector[Vb <: Seq[Int]](idx: AlternativeVector, altVector: Vb) = this.copy(altBinaryVectors = Some(if( altVectors.isDefined ) altBinaryVectors.get + ((idx, altVector)) else immutable.HashMap(idx -> altVector) ))
	/**
	 *
	 */
	def setAltScalarVector[Vs <: Seq[Double]](idx: AlternativeVector, altVector: Vs) = this.copy(altScalarVectors = Some(if( altVectors.isDefined ) altScalarVectors.get + ((idx, altVector)) else immutable.HashMap(idx -> altVector) ))
}