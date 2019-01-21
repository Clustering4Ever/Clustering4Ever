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
trait DataBasedModel[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], T[X] <: Traversable[X]] extends KnnModelModelCz[V, D] {
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
	def knnPredictWithNN(v: V, k: Int): (ClusterID, Seq[(ClusterID, Cz[ID, O, V])]) = {
		dataAsSeq.sortBy{ case (_, cz) => metric.d(v, cz.v) }.take(k).groupBy(_._1).maxBy{ case (clusterID, aggregate) => aggregate.size }
	}
	/**
	 * @return clusterID associate to obj
	 */
	def knnPredict(obj: V, k: Int): ClusterID = knnPredictWithNN(obj, k)._1
	/**
	 * @return sequence of clusterizable with added clusterID
	 */
	def knnPredict[GS[X] <: GenSeq[X]](gs: GS[Cz[ID, O, V]], k: Int)(implicit d: DummyImplicit): GS[Cz[ID, O, V]] = {
		gs.map( cz => cz.addClusterIDs(knnPredict(cz.v, k)) ).asInstanceOf[GS[Cz[ID, O, V]]]
	}
	/**
	 * @return clusterID associate to a GenSeq of object
	 */
	def knnPredict[GS[X] <: GenSeq[X]](gs: GS[V], k: Int): GS[(ClusterID, V)] = gs.map( obj => (knnPredict(obj, k), obj) ).asInstanceOf[GS[(ClusterID, V)]]

}