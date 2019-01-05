package org.clustering4ever.clustering.models
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.math.distances.{Distance, ClusterizableDistance}
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.scala.vectors.GVector
import org.clustering4ever.clusterizables.Clusterizable
/**
 *
 */
trait KnnOrientedModel[O, D <: Distance[O]] extends MetricModel[O, D] {
	/**
	 * Time complexity O(n<sub>trainDS</sub>)
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 */
	def knnPredictWithNN(v: O, k: Int, trainDS: Seq[(ClusterID, O)]): (ClusterID, Seq[(ClusterID, O)]) = trainDS.sortBy{ case (_, vTrain) => metric.d(vTrain, v) }.take(k).groupBy(_._1).maxBy(_._2.size)
	/**
	 * Time complexity O(n<sub>trainDS</sub>)
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 */
	def knnPredict(v: O, k: Int, trainDS: Seq[(ClusterID, O)]): ClusterID = knnPredictWithNN(v, k, trainDS)._1
}
/**
 *
 */
trait KnnOrientedModelClusterizable[V <: GVector[V], D <: Distance[V]] extends KnnOrientedModel[V, D] {
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def knnPredictCzWithNN[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz]](cz: Cz[ID, O, V], k: Int, trainDS: Seq[Cz[ID, O, V]], clusteringNumber: Int): (ClusterID, Seq[Cz[ID, O, V]]) = {
		trainDS.sortBy{ czTrain => metric.d(czTrain.v, cz.v) }.take(k).groupBy(_.clusterIDs(clusteringNumber)).maxBy(_._2.size)
	}
	/**
	 * Time complexity O(c) with c the number of clusters
	 * @return the clusterID of nearest cluster center for a specific point
	 */
	def knnPredictCz[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz]](cz: Cz[ID, O, V], k: Int, trainDS: Seq[Cz[ID, O, V]], clusteringNumber: Int): ClusterID = knnPredictCzWithNN(cz, k, trainDS, clusteringNumber)._1

}
/**
 *
 */
trait KnnOrientedModelLocal[O, D <: Distance[O]] extends KnnOrientedModel[O, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict[GS[X] <: GenSeq[X]](data: GS[O], k: Int, trainDS: Seq[(ClusterID, O)]): GS[(ClusterID, O)] = data.map( v => (knnPredict(v, k, trainDS), v) ).asInstanceOf[GS[(ClusterID, O)]]

}
/**
 *
 */
trait KnnOrientedModelLocalClusterizable[
	V <: GVector[V],
	D <: Distance[V]
] extends KnnOrientedModelLocal[V, D] with KnnOrientedModelClusterizable[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredictCz[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]], k: Int, trainDS: Seq[Cz[ID, O, V]], clusteringNumber: Int): GS[Cz[ID, O, V]] = {
		data.map( cz => cz.addClusterID(knnPredictCz(cz, k, trainDS, clusteringNumber)) ).asInstanceOf[GS[Cz[ID, O, V]]]
	}
}