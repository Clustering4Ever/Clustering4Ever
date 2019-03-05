package org.clustering4ever.vectorizations
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import scala.collection.{mutable, immutable, GenSeq}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.clustering.{ClusteringModelLocalScalar, ClusteringSharedTypes, ClusteringInformationsLocal, ClusteringModelLocal}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.extensibleAlgorithmNature.ClusteringAlgorithmNature
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
trait VectorizationLocalScalar[O, V <: Seq[Double], Self <: VectorizationLocalScalar[O, V, Self]] extends VectorizationGenLocal[O, ScalarVector[V], Self] with VectorizationToRevise[O, ScalarVector[V], Self] {
	this: Self =>
}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 */
trait VectorizationLocalBinary[O, V <: Seq[Int], Self <: VectorizationLocalBinary[O, V, Self]] extends VectorizationGenLocal[O, BinaryVector[V], Self] with VectorizationToRevise[O, BinaryVector[V], Self] {
	this: Self =>
}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 */
final case class EasyVectorizationLocalGenScalar[O, V <: Seq[Double]](
	final val vectorizationID: VectorizationID,
	final val vectorizationFct: Option[O => ScalarVector[V]] = None,
	final val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
	final val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
) extends VectorizationLocalScalar[O, V, EasyVectorizationLocalGenScalar[O, V]] {
	
	final def updateClustering(clusteringIDs: ClusteringNumber*): EasyVectorizationLocalGenScalar[O, V] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)
}
/**
 * @tparam O the raw object from which vectorization are made
 * @tparam V the type of the vector resulting of this vectorization
 */
final case class EasyVectorizationLocalGenBinary[O, V <: Seq[Int]](
	final val vectorizationID: VectorizationID,
	final val vectorizationFct: Option[O => BinaryVector[V]] = None,
	final val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
	final val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
) extends VectorizationLocalBinary[O, V, EasyVectorizationLocalGenBinary[O, V]] {
	
	final def updateClustering(clusteringIDs: ClusteringNumber*): EasyVectorizationLocalGenBinary[O, V] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)
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
