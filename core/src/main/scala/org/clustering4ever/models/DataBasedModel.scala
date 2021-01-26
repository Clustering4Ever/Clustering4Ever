package org.clustering4ever.models

/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances.Distance
import org.clustering4ever.roottraits.{Clusterizable, GVector}

import scala.collection.{GenSeq, mutable}
import scala.language.higherKinds
/**
 * @tparam V
 * @tparam D
 */
trait DataBasedModel[V <: GVector[V], D <: Distance[V]] extends KnnModelModelCz[V, D] {
	/**
	 * The whole dataset sorted by clusterizable IDs as a collection [(ID, Vector, ClusterID)]
	 */
	val datapointWithClusterIDSortedByPointID: mutable.ArrayBuffer[(Long, (V, ClusterID))]
	/**
	 * @return clusterID associate to obj and its knn containing (ClusterID, Seq[(ID, Vector)])
	 */
	final def knnPredictWithNN(v: V, k: Int): (ClusterID, Seq[(Long, V)]) = {
		val (clusterID, knn) = datapointWithClusterIDSortedByPointID.sortBy{ case (_, (v2, _)) => metric.d(v, v2) }.take(k).groupBy(_._2._2).maxBy{ case (clusterID, aggregate) => aggregate.size }
		(clusterID, knn.map{ case (id, (v, _)) => (id, v) })
	}
	/**
	 * @return clusterID associate to given Vector
	 */
	final def knnPredict(v: V, k: Int): ClusterID = knnPredictWithNN(v, k)._1
	/**
	 * @return sequence of clusterizable with added clusterID
	 */
	final def knnPredict[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](gs: GS[Cz[O, V]], k: Int)(implicit d: DummyImplicit): GS[Cz[O, V]] = {
		gs.map( cz => cz.addClusterIDs(knnPredict(cz.v, k)) ).asInstanceOf[GS[Cz[O, V]]]
	}
	/**
	 * @return clusterID associate to a GenSeq of object
	 */
	final def knnPredict[GS[X] <: GenSeq[X]](gs: GS[V], k: Int): GS[(ClusterID, V)] = gs.map( obj => (knnPredict(obj, k), obj) ).asInstanceOf[GS[(ClusterID, V)]]

}