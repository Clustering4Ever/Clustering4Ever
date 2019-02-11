package org.clustering4ever.clustering.models
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.math.distances.{GenericDistance, Distance, GSimpleVectorDistance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.vectors.{GVector, GSimpleVector, ScalarVector, BinaryVector, MixedVector}
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.clusterizables.Clusterizable
/**
 * @tparam V
 * @tparam D
 */
trait KnnModelModel[V <: GVector[V], D <: Distance[V]] extends MetricModel[V, D] {
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors with its KNN
	 */
	def knnPredictWithNN(v: V, k: Int, trainDS: Seq[(ClusterID, V)]): (ClusterID, Seq[(ClusterID, V)]) = {
		trainDS.sortBy{ case (_, vTrain) => metric.d(vTrain, v) }.take(k).groupBy(_._1).maxBy(_._2.size)
	}
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 */
	def knnPredict(v: V, k: Int, trainDS: Seq[(ClusterID, V)]): ClusterID = knnPredictWithNN(v, k, trainDS)._1
}
/**
 * @tparam T
 * @tparam V
 * @tparam SV
 * @tparam D
 */
trait KnnModelSimpleV[T, V <: Seq[T], SV <: GSimpleVector[T, V, SV], D <: GSimpleVectorDistance[T, V, SV]] extends KnnModelModel[SV, D] {
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 */
	def knnPredict(v: V, k: Int, trainDS: Seq[(ClusterID, V)]): ClusterID = knnPredict(v, k, trainDS)
}
/**
 * @tparam V
 * @tparam D
 */
trait KnnModelModelReal[V <: Seq[Double], D <: ContinuousDistance[V]] extends KnnModelSimpleV[Double, V, ScalarVector[V], D]
/**
 * @tparam V
 * @tparam D
 */
trait KnnModelModelBinary[V <: Seq[Int], D <: BinaryDistance[V]] extends KnnModelSimpleV[Int, V, BinaryVector[V], D]
/**
 * @tparam Vb
 * @tparam Vs
 * @tparam D
 */
trait KnnModelModelMixed[Vb <: Seq[Int], Vs <: Seq[Double], D <: MixedDistance[Vb, Vs]] extends KnnModelModel[MixedVector[Vb, Vs], D] {
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 */
	final def knnPredict(v: (Vb, Vs), k: Int, trainDS: Seq[(ClusterID, (Vb, Vs))]): ClusterID = knnPredict(v, k, trainDS)
}
/**
 * @tparam V
 * @tparam D
 */
trait KnnModelModelCz[V <: GVector[V], D <: Distance[V]] extends KnnModelModel[V, D] {
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 */
	final def knnPredictWithNN[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](cz: Cz[O, V], k: Int, trainDS: Seq[Cz[O, V]], clusteringNumber: Int): (ClusterID, Seq[Cz[O, V]]) = {
		trainDS.sortBy{ czTrain => metric.d(czTrain.v, cz.v) }.take(k).groupBy(_.clusterIDs(clusteringNumber)).maxBy(_._2.size)
	}
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 */
	final def knnPredict[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](cz: Cz[O, V], k: Int, trainDS: Seq[Cz[O, V]], clusteringNumber: Int): ClusterID = knnPredictWithNN(cz, k, trainDS, clusteringNumber)._1

}
/**
 * @tparam T
 * @tparam V
 * @tparam SV
 * @tparam D
 */
trait KnnModelModelLocalSimpleV[T, V <: Seq[T], SV <: GSimpleVector[T, V, SV], D <: GSimpleVectorDistance[T, V, SV]] extends KnnModelSimpleV[T, V, SV, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.d.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	final def knnPredict[GS[X] <: GenSeq[X]](data: GS[V], k: Int, trainDS: Seq[(ClusterID, V)]): GS[(ClusterID, V)] = data.map( v => (knnPredict(v, k, trainDS), v) ).asInstanceOf[GS[(ClusterID, V)]]

}
/**
 * @tparam V
 * @tparam D
 */
trait KnnModelModelLocal[V <: GVector[V], D <: Distance[V]] extends KnnModelModel[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.d.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	final def knnPredict[GS[X] <: GenSeq[X]](data: GS[V], k: Int, trainDS: Seq[(ClusterID, V)]): GS[(ClusterID, V)] = data.map( v => (knnPredict(v, k, trainDS), v) ).asInstanceOf[GS[(ClusterID, V)]]

}
/**
 * @tparam V
 * @tparam D
 */
trait KnnModelModelLocalCz[V <: GVector[V], D <: Distance[V]] extends KnnModelModelLocal[V, D] with KnnModelModelCz[V, D] {
	/**
	 * Time complexity O(d.n<sub>data</sub>.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size, n<sub>data</sub> is the size of dataset to predict
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	final def knnPredict[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]], k: Int, trainDS: Seq[Cz[O, V]], clusteringNumber: Int): GS[Cz[O, V]] = {
		data.map( cz => cz.addClusterIDs(knnPredict(cz, k, trainDS, clusteringNumber)) ).asInstanceOf[GS[Cz[O, V]]]
	}
}