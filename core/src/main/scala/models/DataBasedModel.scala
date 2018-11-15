package org.clustering4ever.scala.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{GenSeq, mutable}
import spire.math.{Numeric => SNumeric}
import org.clustering4ever.clustering.ClusteringModel
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
/**
 *
 */
trait DataBasedModel[@specialized(Int, Long) ID, O, D <: Distance[O]] extends ClusteringModel {

	val data: scala.collection.Map[ClusterID, scala.collection.Traversable[(ID, O)]]
	val metric: D
	/**
	 *
	 */
	lazy val dataAsSeq: Seq[(ClusterID, (ID, O))] = data.toSeq.flatMap{ case (clusterID, values) => values.map{ case (id, vector) => (clusterID, (id, vector)) } }
	/**
	 * @return clusterID associate to obj and its knn containing (ClusterID, (ID, obj))
	 */
	def knn(obj: O, k: Int): (ClusterID, Seq[(ClusterID, (ID, O))]) = dataAsSeq.sortBy{ case (_, (_, obj2)) => metric.d(obj, obj2) }.take(k).groupBy(_._1).maxBy{ case (clusterID, aggregate) => aggregate.size }
	/**
	 * @return clusterID associate to obj
	 */
	def knnPredict(obj: O, k: Int): ClusterID = knn(obj, k)._1
	/**
	 * @return clusterID associate to a GenSeq of object
	 */
	def knnPredict(genSeq: GenSeq[O], k: Int): GenSeq[(ClusterID, O)] = genSeq.map( obj => (knnPredict(obj, k), obj) )

}