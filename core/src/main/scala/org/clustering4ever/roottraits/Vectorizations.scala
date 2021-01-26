package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */
import org.clustering4ever.clusteringtraits.{ClusteringInformationsLocal, ClusteringSharedTypes}
import org.clustering4ever.roottraits.ClusteringNumberType._
import org.clustering4ever.roottraits.VectorizationIDTypes._

import scala.collection.{immutable, mutable}
import scala.language.higherKinds
/**
 *
 */
trait VectorizationAncestor[Self <: VectorizationAncestor[Self]] extends ClusteringSharedTypes {
	/**
	 * Guaranty for inheriting
	 */
	this: Self =>
	/**
	 * ID of this vectorization
	 */
	val vectorizationID: VectorizationID
	/**
	 * Used mapping to obtain this vectorization in vectorizations HMap in various clustering chaining classes
	 */
	val vectoMapping = VectorizationMapping[VectorizationID, Self]
}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 * @tparam Self the concrete implementation of this vectorization
 */
trait Vectorization[O, V <: GVector[V], Self <: Vectorization[O, V, Self]] extends VectorizationAncestor[Self] {
	this: Self =>
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
	final val vMapping = VMapping[VectorizationID, V]
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
trait VectorizationGenLocal[O, V <: GVector[V], Self <: VectorizationGenLocal[O, V, Self]] extends Vectorization[O, V, Self] with VectorizationToRevise[O, V, Self] {
	this: Self =>
	/**
	 *
	 */
	final val runnedAlgorithms: mutable.ArrayBuffer[(ClusteringNumber, ClusteringAlgorithmNature)] = mutable.ArrayBuffer.empty[(ClusteringNumber, ClusteringAlgorithmNature)]
	/**
	 *
	 */
	final def updateAlgorithms(cnWithAlgo: (ClusteringNumber, ClusteringAlgorithmNature)*): Unit = runnedAlgorithms ++= cnWithAlgo
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
	final val informationMapping = ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[O, V, Self]]
}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 */
trait VectorizationLocalScalar[O, Self <: VectorizationLocalScalar[O, Self]] extends VectorizationGenLocal[O, ScalarVector, Self] with VectorizationToRevise[O, ScalarVector, Self] {
	this: Self =>
}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 */
trait VectorizationLocalBinary[O, Self <: VectorizationLocalBinary[O, Self]] extends VectorizationGenLocal[O, BinaryVector, Self] with VectorizationToRevise[O, BinaryVector, Self] {
	this: Self =>
}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 */
final case class EasyVectorizationLocalGenScalar[O](
	final val vectorizationID: VectorizationID,
	final val vectorizationFct: Option[O => ScalarVector] = None,
	final val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
	final val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
) extends VectorizationLocalScalar[O, EasyVectorizationLocalGenScalar[O]] {
	
	final def updateClustering(clusteringIDs: ClusteringNumber*): EasyVectorizationLocalGenScalar[O] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)
}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 */
final case class EasyVectorizationLocalGenBinary[O](
	final val vectorizationID: VectorizationID,
	final val vectorizationFct: Option[O => BinaryVector] = None,
	final val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
	final val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
) extends VectorizationLocalBinary[O, EasyVectorizationLocalGenBinary[O]] {
	
	final def updateClustering(clusteringIDs: ClusteringNumber*): EasyVectorizationLocalGenBinary[O] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)
}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 */
final case class EasyVectorizationLocal[O, V <: GVector[V]](
	final val vectorizationID: VectorizationID,
	final val vectorizationFct: Option[O => V] = None,
	final val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
	final val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
) extends VectorizationLocal[O, V, EasyVectorizationLocal] {

	final def updateClustering(clusteringIDs: ClusteringNumber*): EasyVectorizationLocal[O, V] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)

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
