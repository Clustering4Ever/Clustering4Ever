package org.clustering4ever.vectorizations
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.vectors.{GVector, BinaryVector}
import scala.collection.{mutable, immutable, GenSeq}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.clustering.{ClusteringSharedTypes, ClusteringInformationsLocal, ClusteringModelLocal, ConcreteSpecificClusteringInformationsLocalNew}
import org.clustering4ever.clusterizables.Clusterizable
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 * @tparam Self the concrete implementation of this vectorization
 */
trait Vectorization[O, V <: GVector[V], Self <: Vectorization[O, V, Self]] extends ClusteringSharedTypes {
	/**
	 * Guaranty for inheriting
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
trait VectorizationToRevise[O, V <: GVector[V], Self <: VectorizationToRevise[O, V, Self]] extends Vectorization[O, V, Self] {
	this: Self =>
	/**
	 * Update clusteringNumbers field with given clusteringNumbers
	 */
	def updateClustering(clusteringIDs: ClusteringNumber*): Self
	/**
	 * IDs of realized clustering with this vectorization
	 */
	val clusteringNumbers: immutable.HashSet[ClusteringNumber]
}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 */
trait VectorizationGenLocal[O, V <: GVector[V], Self <: VectorizationGenLocal[O, V, Self]] extends Vectorization[O, V, Self] {
	this: Self =>
}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 */
trait VectorizationLocal[O, V <: GVector[V], Self[A, B <: GVector[B]] <: VectorizationLocal[A, B, Self]] extends VectorizationGenLocal[O, V, Self[O, V]] with VectorizationToRevise[O, V, Self[O, V]] {
	this: Self[O, V] =>
	/**
	 * InformationMapping to obtain ClusteringInformationsLocal object for this given vectorization in ClusterAnalysisLocal HMap
	 */
	val informationMapping = ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[O, V, Self]]
}
/**
 *
 */
trait VectorizationWithAlgorithmGenLocal[O, V <: GVector[V], CM <: ClusteringModelLocal[V], Self <: VectorizationWithAlgorithmGenLocal[O, V, CM, Self]] extends VectorizationGenLocal[O, V, Self] {
	this: Self =>
}
/**
 *
 */
trait VectorizationWithAlgorithmLocal[O, V <: GVector[V], CM <: ClusteringModelLocal[V], Self <: VectorizationWithAlgorithmLocal[O, V, CM, Self]] extends VectorizationWithAlgorithmGenLocal[O, V, CM, Self] {

	this: Self =>
	/**
	 * Buffer of models obtained after the clustering algorithm corresponding to this Vectorization
	 */
	val models: mutable.ArrayBuffer[CM] = mutable.ArrayBuffer.empty[CM]
	/**
	 *
	 */
	def addModels(newModels: CM*): Unit = models ++= newModels

}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 */
case class EasyVectorizationLocal[O, V <: GVector[V]](
	val vectorizationID: VectorizationID,
	val vectorizationFct: Option[O => V] = None,
	val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
	val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
) extends VectorizationLocal[O, V, EasyVectorizationLocal] {

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
