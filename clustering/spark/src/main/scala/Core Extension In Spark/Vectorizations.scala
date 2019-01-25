package org.clustering4ever.vectorizations
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.immutable
import org.apache.spark.rdd.RDD
import org.clustering4ever.vectors.GVector
import org.clustering4ever.shapeless.ClusteringInformationsMapping
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
trait VectorizationDistributed[O, V <: GVector[V], Self <: VectorizationDistributed[O, V, Self]] extends Vectorization[O, V, Self] {
	/**
	 *
	 */
	this: Self =>
	/**
	 *
	 */
	def getInformationMapping[ID, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](cz: Option[RDD[Cz[ID, O, V]]] = None): ClusteringInformationsMapping[VectorizationID, Self] = {
		ClusteringInformationsMapping[VectorizationID, Self]
	}
}
/**
 *
 */
case class EasyVectorizationDistributed[O, V <: GVector[V]] (
	val vectorizationID: VectorizationID,
	val vectorizationFct: Option[O => V] = None,
	val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
	val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
) extends VectorizationDistributed[O, V, EasyVectorizationDistributed[O, V]] {
	/**
	 *
	 */
	def updateClustering(clusteringIDs: Int*): EasyVectorizationDistributed[O, V] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)
}

