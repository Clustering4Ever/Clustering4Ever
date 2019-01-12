package org.clustering4ever.spark.clustering.kcenters
/**
 * Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.mutable
import org.apache.spark.rdd.RDD
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.models.CenterOrientedModelDistributedCz
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.clustering.ClusteringModelDistributed
import org.clustering4ever.vectors.GVector
/**
 *
 */
case class KCentersModel[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V]](val centers: mutable.HashMap[Int, V], val metric: D)(implicit val ct: ClassTag[Cz[ID, O, V]]) extends CenterOrientedModelDistributedCz[V, D] with ClusteringModelDistributed[ID, O, V, Cz] {
	/**
	 *
	 */
	def obtainClustering(data: RDD[Cz[ID, O, V]]): RDD[Cz[ID, O, V]] = centerPredictCz(data)

}