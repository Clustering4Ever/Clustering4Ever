package org.clustering4ever.sparkcoreextension

/**
 * @author Beck Gaël
 */
import org.clustering4ever.roottraits.{ClusteringInformationsMapping, GVector, VectorizationToRevise}
import org.clustering4ever.roottraits.VectorizationIDTypes._

import scala.collection.immutable
import scala.language.higherKinds
/**
 *
 */
trait VectorizationDistributed[O, V <: GVector[V], Self[A, B <: GVector[B]] <: VectorizationDistributed[A, B, Self]] extends VectorizationToRevise[O, V, Self[O, V]] {

	this: Self[O, V] =>

	final val informationMapping = ClusteringInformationsMapping[VectorizationID, Self[O, V]]

}
/**
 *
 */
final case class EasyVectorizationDistributed[O, V <: GVector[V]] (
	val vectorizationID: VectorizationID,
	val vectorizationFct: Option[O => V] = None,
	val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
	val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
) extends VectorizationDistributed[O, V, EasyVectorizationDistributed] {
	/**
	 *
	 */
	final def updateClustering(clusteringIDs: Int*): EasyVectorizationDistributed[O, V] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)
}

