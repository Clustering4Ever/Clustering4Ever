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
import org.clustering4ever.clustering.ClusteringModelCz
import org.clustering4ever.vectors.GVector
/**
 *
 */
class KCentersModel[V <: GVector[V], D <: Distance[V]](val centers: mutable.HashMap[Int, V], val metric: D) extends CenterOrientedModelDistributedCz[V, D] with ClusteringModelCz[V, RDD] {
	/**
	 *
	 */
	def obtainClustering[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): RDD[Cz[ID, O, V]] = centerPredictCz(data)

}