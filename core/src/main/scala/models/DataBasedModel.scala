package org.clustering4ever.clustering.models
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{GenSeq, mutable, Traversable}
import org.clustering4ever.clustering.GenericClusteringModel
import org.clustering4ever.math.distances.{GenericDistance, Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.identifiables.IdentifiedRawObject
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
trait GenericDataBasedModel[ID, O, D <: GenericDistance[O], T[X] <: Traversable[X], IRO[A, B] <: IdentifiedRawObject[A, B]] extends GenericKnnOrientedModel[O, D] {
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
/**
 *
 */
trait DataBasedModel[ID, V <: GVector[V], D <: Distance[V], T[X] <: Traversable[X], IRO[A, B] <: IdentifiedRawObject[A, B]] extends GenericDataBasedModel[ID, V, D, T, IRO]
/**
 *
 */
trait DataBasedModelCz[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], T[X] <: Traversable[X]] extends KnnOrientedModelClusterizable[V, D] {
	/**
	 *
	 */
	val data: scala.collection.Map[ClusterID, T[Cz[ID, O, V]]]
	/**
	 *
	 */
	val metric: D
	/**
	 *
	 */
	lazy val dataAsSeq: Seq[(ClusterID, Cz[ID, O, V])] = data.toSeq.flatMap{ case (clusterID, values) => values.map((clusterID, _)) }
	/**
	 * @return clusterID associate to obj and its knn containing (ClusterID, (ID, obj))
	 */
	def knnPredictWithNN(obj: V, k: Int): (ClusterID, Seq[(ClusterID, Cz[ID, O, V])]) = {
		dataAsSeq.sortBy{ case (_, cz) => metric.d(obj, cz.v) }.take(k).groupBy(_._1).maxBy{ case (clusterID, aggregate) => aggregate.size }
	}
	/**
	 * @return clusterID associate to obj
	 */
	def knnPredict(obj: V, k: Int): ClusterID = knnPredictWithNN(obj, k)._1
	/**
	 * @return clusterID associate to a GenSeq of object
	 */
	def knnPredict[GS[X] <: GenSeq[X]](genSeq: GS[V], k: Int): GS[(ClusterID, V)] = genSeq.map( obj => (knnPredict(obj, k), obj) ).asInstanceOf[GS[(ClusterID, V)]]

}