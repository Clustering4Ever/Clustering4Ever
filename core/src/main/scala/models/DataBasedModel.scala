package org.clustering4ever.clustering.models
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{GenSeq, mutable, Traversable}
import org.clustering4ever.clustering.ClusteringModel
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.identifiables.IdentifiedRawObject
/**
 *
 */
trait DataBasedModel[ID, O, D <: Distance[O], T[X] <: Traversable[X], IRO[A, B] <: IdentifiedRawObject[A, B]] extends KnnOrientedModel[O, D] {
	/**
	 *
	 */
	val data: scala.collection.Map[ClusterID, T[IRO[ID, O]]]
	/**
	 *
	 */
	val metric: D
	/**
	 *
	 */
	lazy val dataAsSeq: Seq[(ClusterID, IRO[ID, O])] = data.toSeq.flatMap{ case (clusterID, values) => values.map((clusterID, _)) }
	/**
	 * @return clusterID associate to obj and its knn containing (ClusterID, (ID, obj))
	 */
	def knnPredictWithNN(obj: O, k: Int): (ClusterID, Seq[(ClusterID, IRO[ID, O])]) = {
		dataAsSeq.sortBy{ case (_, iro) => metric.d(obj, iro.o) }.take(k).groupBy(_._1).maxBy{ case (clusterID, aggregate) => aggregate.size }
	}
	/**
	 * @return clusterID associate to obj
	 */
	def knnPredict(obj: O, k: Int): ClusterID = knnPredictWithNN(obj, k)._1
	/**
	 * @return clusterID associate to a GenSeq of object
	 */
	def knnPredict[GS[X] <: GenSeq[X]](genSeq: GS[O], k: Int): GS[(ClusterID, O)] = genSeq.map( obj => (knnPredict(obj, k), obj) ).asInstanceOf[GS[(ClusterID, O)]]

}