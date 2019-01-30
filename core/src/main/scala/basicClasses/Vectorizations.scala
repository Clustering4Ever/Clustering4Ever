package org.clustering4ever.vectorizations
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.vectors.{GVector, BinaryVector}
import scala.collection.{immutable, GenSeq}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.clustering.{ClusteringSharedTypes, ClusteringInformationsLocal}
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
trait Vectorization[O, V <: GVector[V], Self <: Vectorization[O, V, Self]] extends ClusteringSharedTypes {
	/**
	 *
	 */
	this: Self =>
	/**
	 * ID of this vectorization
	 */
	val vectorizationID: VectorizationID
	/**
	 * Option of the function which transform the raw object toward a GVector descendant
	 */
	val vectorizationFct: Option[O => V]
	/**
	 * Names of features of the corresponding output vector
	 */
	val outputFeaturesNames: immutable.Vector[String]
	/**
	 * IDs of realized clustering with this vectorization
	 */
	val clusteringNumbers: immutable.HashSet[ClusteringNumber]
	/**
	 * Update clusteringNumbers field with given clusteringNumbers
	 */
	def updateClustering(clusteringIDs: ClusteringNumber*): Self
	/**
	 * Used mapping to obtain this vectorization vector on vectorized HMAP field in clusterizable
	 */
	val vMapping = VMapping[VectorizationID, V]
	/**
	 * Used mapping to obtain this vectorization in vectorizations HMap in various clustering chaining classes
	 */
	val vectoMapping = VectorizationMapping[VectorizationID, Self]
}
/**
 *
 */
trait VectorizationLocal[O, V <: GVector[V], Self[A, B <: GVector[B]] <: VectorizationLocal[A, B, Self]]  extends Vectorization[O, V, Self[O, V]] {
	/**
	 *
	 */
	this: Self[O, V] =>
	/**
	 * @return InformationMapping to obtain ClusteringInformationsLocal object for this given vectorization in ClusterAnalysisLocal HMap
	 */
	def getInformationMapping[ID, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](cz: Option[GS[Cz[ID, O, V]]] = None): ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[O, V, Self]] = {
		ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[O, V, Self]]
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
	 * Update clusteringNumbers field with given clusteringNumbers
	 */
	def updateClustering(clusteringIDs: ClusteringNumber*): EasyVectorizationLocal[O, V] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)

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
