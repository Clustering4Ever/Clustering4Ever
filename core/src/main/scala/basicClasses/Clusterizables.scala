package clustering4ever.scala.clusterizables
/**
 * @author Beck Gaël
 */
import clustering4ever.scala.vectorizables.{Vectorizable, MixtVectorizable, RealVectorizable, BinaryVectorizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
import spire.math.{Numeric => SNumeric}
/**
 *
 */
abstract class Clusterizable[@specialized(Int, Long) ID: Numeric, V](val id: ID, val vectorizable: Vectorizable[V]) extends Serializable {
	final lazy val vector: V = vectorizable.toVector
}
/**
 *
 */
abstract class ClusterizableExt[ID: Numeric, V, Self <: ClusterizableExt[ID, V, Self]](
	id: ID, 
	vectorizable: Vectorizable[V],
	val v2: Option[V] = None,
	val clusterID: Option[Int] = None
) extends Clusterizable[ID, V](id, vectorizable) {

	def setV2(newV2: V): Self

	def setClusterID(newCID: Int): Self
}
/**
 *
 */
abstract class ClusterizableExtVectors[
	ID: Numeric,
	@specialized(Int, Double) N: SNumeric,
	V <: Seq[N],
	Self <: ClusterizableExtVectors[ID, N, V, Self]
](
	id: ID, 
	vectorizable: Vectorizable[V],
	v2: Option[V] = None,
	clusterID: Option[Int] = None
) extends ClusterizableExt[ID, V, Self](id, vectorizable, v2, clusterID) {

	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + vector.hashCode
		result = prime * result + v2.hashCode
		result = prime * result + clusterID.hashCode
		result = prime * result + id.hashCode
		result
	}
}
/**
 *
 */
abstract class ClusterizableExtMixt[
	ID: Numeric,
	Vb <: Seq[Int],
	Vs <: Seq[Double],
	Self <: ClusterizableExtMixt[ID, Vb, Vs, Self]
](
	id: ID, 
	vectorizable: Vectorizable[BinaryScalarVector[Vb, Vs]],
	v2: Option[BinaryScalarVector[Vb, Vs]] = None,
	clusterID: Option[Int] = None
) extends ClusterizableExt[ID, BinaryScalarVector[Vb, Vs], Self](id, vectorizable) {

	override def hashCode(): Int = {
		val prime = 31
		var result = 1
		result = prime * result + vector.binary.hashCode
		result = prime * result + vector.scalar.hashCode
		result = prime * result + v2.hashCode
		result = prime * result + clusterID.hashCode
		result = prime * result + id.hashCode
		result
	}
}
/**
 *
 */
abstract class RealClusterizable[ID: Numeric, O, V <: Seq[Double], Self <: RealClusterizable[ID, O, V, Self]](
	id: ID,
	vectorizable: RealVectorizable[O, V],
	v2: Option[V] = None,
	clusterID: Option[Int] = None
) extends ClusterizableExtVectors[ID, Double, V, Self](id, vectorizable, v2, clusterID)
/**
 *
 **/
case class SimpleRealClusterizable[ID: Numeric, O, V <: Seq[Double]](
	override val id: ID,
	override val vectorizable: RealVectorizable[O, V],
	override val v2: Option[V] = None,
	override val clusterID: Option[Int] = None
) extends RealClusterizable[ID, O, V, SimpleRealClusterizable[ID, O, V]](id, vectorizable, v2, clusterID) {

	override def canEqual(a: Any): Boolean = a.isInstanceOf[SimpleRealClusterizable[ID, O, V]]

	override def equals(that: Any): Boolean = {
		that match {
		  case that: SimpleRealClusterizable[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}

	def setV2(newV2: V): SimpleRealClusterizable[ID, O, V] = this.copy(v2 = Some(newV2))

	def setClusterID(newCID: Int): SimpleRealClusterizable[ID, O, V] = this.copy(clusterID = Some(newCID))
}
/**
 *
 */
abstract class BinaryClusterizable[ID: Numeric, O, V <: Seq[Int], Self <: BinaryClusterizable[ID, O, V, Self]](
	id: ID,
	vectorizable: BinaryVectorizable[O, V],
	v2: Option[V] = None,
	clusterID: Option[Int] = None
) extends ClusterizableExtVectors[ID, Int, V, Self](id, vectorizable, v2, clusterID)
/**
 *
 */
case class SimpleBinaryClusterizable[ID: Numeric, O, V <: Seq[Int]](
	override val id: ID,
	override val vectorizable: BinaryVectorizable[O, V],
	override val v2: Option[V] = None,
	override val clusterID: Option[Int] = None
) extends BinaryClusterizable[ID, O, V, SimpleBinaryClusterizable[ID, O, V]](id, vectorizable, v2, clusterID) {

	def canEqual(a: Any): Boolean = a.isInstanceOf[SimpleBinaryClusterizable[ID, O, V]]

	override def equals(that: Any): Boolean = {
		that match {
		  case that: SimpleBinaryClusterizable[ID, O, V] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}

	def setV2(newV2: V): SimpleBinaryClusterizable[ID, O, V] = this.copy(v2 = Some(newV2))

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
	v2: Option[BinaryScalarVector[Vb, Vs]] = None,
	clusterID: Option[Int] = None
) extends ClusterizableExtMixt[ID, Vb, Vs, Self](id, vectorizable, v2, clusterID)
/**
 * Basic clusterizable for Mixt Vectors => (V[Int], V[Double]) 
 */
case class SimpleMixtClusterizable[ID: Numeric, O, Vb <: Seq[Int], Vs <: Seq[Double]](
	override val id: ID,
	override val vectorizable: MixtVectorizable[O, Vb, Vs],
	override val v2: Option[BinaryScalarVector[Vb, Vs]] = None,
	override val clusterID: Option[Int] = None
) extends MixtClusterizable[ID, O, Vb, Vs, SimpleMixtClusterizable[ID, O, Vb, Vs]](id, vectorizable, v2, clusterID) {

	def canEqual(a: Any): Boolean = a.isInstanceOf[SimpleMixtClusterizable[ID, O, Vb, Vs]]

	override def equals(that: Any): Boolean = {
		that match {
		  case that: SimpleMixtClusterizable[ID, O, Vb, Vs] => that.canEqual(this) && that.hashCode == this.hashCode
		  case _ => false
		}
	}
	
	def setV2(newV2: BinaryScalarVector[Vb, Vs]): SimpleMixtClusterizable[ID, O, Vb, Vs] = this.copy(v2 = Some(newV2))

	def setClusterID(newCID: Int): SimpleMixtClusterizable[ID, O, Vb, Vs] = this.copy(clusterID = Some(newCID))
}