package org.clustering4ever.vectorizations
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.vectors.{GVector, BinaryVector}
import scala.collection.{immutable, GenSeq}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.clustering.{ClusteringCommons, ClusteringInformationsLocal}
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
sealed trait VectorizationNature extends Serializable
/**
 *
 */
object Scalar extends VectorizationNature
/**
 *
 */
object Binary extends VectorizationNature
/**
 *
 */
object Mixt extends VectorizationNature
/**
 *
 */
object Other extends VectorizationNature
/**
 *
 */
object Default extends VectorizationNature
/**
 *
 */
trait Vectorization[O, V <: GVector[V], Self[A, B <: GVector[B]] <: Vectorization[A, B, Self]]  extends ClusteringCommons {
	/**
	 *
	 */
	val vectorizationID: VectorizationID
	/**
	 *
	 */
	val vectorizationFct: Option[O => V]
	/**
	 *
	 */
	val outputFeaturesNames: immutable.Vector[String]
	/**
	 *
	 */
	val clusteringNumbers: immutable.HashSet[ClusterID]
	/**
	 *
	 */
	def updateClustering(clusteringIDs: Int*): Self[O, V]
	/**
	 *
	 */
	val vMapping = VMapping[VectorizationID, V]
	/**
	 *
	 */
	val vectoMapping = VectorizationMapping[VectorizationID, Self[O, V]]
}
/**
 *
 */
trait VectorizationLocal[O, V <: GVector[V], Self[A, B <: GVector[B]] <: VectorizationLocal[A, B, Self]]  extends Vectorization[O, V, Self] {
	/**
	 *
	 */
	def getInformationMapping[ID, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](cz: Option[GS[Cz[ID, O, V]]] = None): ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[ID, O, V, Cz, Self, GS]] = {
		ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[ID, O, V, Cz, Self, GS]]
	}
}
/**
 *
 */
case class EasyVectorizationLocal[O, V <: GVector[V]](
	val vectorizationID: VectorizationID,
	val vectorizationFct: Option[O => V] = None,
	val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
	val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
) extends VectorizationLocal[O, V, EasyVectorizationLocal] {
	/**
	 *
	 */
	def updateClustering(clusteringIDs: Int*): EasyVectorizationLocal[O, V] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)

}
/**
 *
 */
// case class EasyVectorizationBinaryLocal[O, V <: Seq[Int]](
// 	val vectorizationID: VectorizationID,
// 	val vectorizationFct: Option[O => BinaryVector[V]] = None,
// 	val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
// 	val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
// ) extends VectorizationLocal[O, BinaryVector[V], EasyVectorizationBinaryLocal] {
// 	/**
// 	 *
// 	 */
// 	def updateClustering(clusteringIDs: Int*): EasyVectorizationBinaryLocal[O, V] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)

// }
