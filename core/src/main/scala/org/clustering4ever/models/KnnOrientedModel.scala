package org.clustering4ever.models

/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances._
import org.clustering4ever.roottraits._

import scala.collection.GenSeq
import scala.language.higherKinds
/**
 * @tparam V
 * @tparam D
 */
trait KnnModelModel[V <: GVector[V], D <: Distance[V]] extends MetricModel[V, D] {
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors with its KNN
	 */
	final def knnPredictWithNN(v: V, k: Int, trainDS: Seq[(ClusterID, V)]): (ClusterID, Seq[(ClusterID, V)]) = {
		trainDS.sortBy{ case (_, vTrain) => metric.d(vTrain, v) }.take(k).groupBy(_._1).maxBy(_._2.size)
	}
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 */
	final def knnPredict(v: V, k: Int, trainDS: Seq[(ClusterID, V)]): ClusterID = knnPredictWithNN(v, k, trainDS)._1
}
/**
 * @tparam T
 * @tparam V
 * @tparam SV
 * @tparam D
 */
trait KnnModelSimpleV[N, SV <: GSimpleVector[N, SV], D <: GSimpleVectorDistance[N, SV]] extends KnnModelModel[SV, D] {
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors with its KNN
	 */
	final def knnPredictWithNN(v: Array[N], k: Int, trainDS: Seq[(ClusterID, Array[N])]): (ClusterID, Seq[(ClusterID, Array[N])]) = {
		trainDS.sortBy{ case (_, vTrain) => metric.dRaw(vTrain, v) }.take(k).groupBy(_._1).maxBy(_._2.size)
	}
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 */
	final def knnPredict(v: Array[N], k: Int, trainDS: Seq[(ClusterID, Array[N])]): ClusterID = knnPredictWithNN(v, k, trainDS)._1
}
/**
 * @tparam V
 * @tparam D
 */
trait KnnModelModelScalar[D <: ContinuousDistance] extends KnnModelSimpleV[Double, ScalarVector, D]
/**
 * @tparam V
 * @tparam D
 */
trait KnnModelModelBinary[D <: BinaryDistance] extends KnnModelSimpleV[Int, BinaryVector, D]
/**
 * @tparam Vb
 * @tparam Vs
 * @tparam D
 */
trait KnnModelModelMixed[D <: MixedDistance] extends KnnModelModel[MixedVector, D] {
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors with its KNN
	 */
	final def knnPredictWithNN(v: (Array[Int], Array[Double]), k: Int, trainDS: Seq[(ClusterID, (Array[Int], Array[Double]))]): (ClusterID, Seq[(ClusterID, (Array[Int], Array[Double]))]) = {
		trainDS.sortBy{ case (_, vTrain) => metric.dRaw(vTrain, v) }.take(k).groupBy(_._1).maxBy(_._2.size)
	}
	/**
	 * Time complexity O(d.n<sub>trainDS</sub>), d works for dimentionality and n<sub>trainDS</sub> is the training dataset size
	 * @return the clusterID of cluster which has the most number of vectors closest from a specific point among its k nearest neighbors
	 */
	final def knnPredict(v: (Array[Int], Array[Double]), k: Int, trainDS: Seq[(ClusterID, (Array[Int], Array[Double]))]): ClusterID = knnPredictWithNN(v, k, trainDS)._1
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
	final def knnPredict[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](cz: Cz[O, V], k: Int, trainDS: Seq[Cz[O, V]], clusteringNumber: Int): ClusterID =  {
		knnPredictWithNN(cz, k, trainDS, clusteringNumber)._1
	}
}
/**
 * @tparam T
 * @tparam V
 * @tparam SV
 * @tparam D
 */
trait KnnModelModelLocalSimpleV[N, SV <: GSimpleVector[N, SV], D <: GSimpleVectorDistance[N, SV]] extends KnnModelSimpleV[N, SV, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.d.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	final def knnPredict[GS[X] <: GenSeq[X]](data: GS[Array[N]], k: Int, trainDS: Seq[(ClusterID, Array[N])]): GS[(ClusterID, Array[N])] = {
		data.map( v => (knnPredict(v, k, trainDS), v) ).asInstanceOf[GS[(ClusterID, Array[N])]]
	}
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
	final def knnPredict[GS[X] <: GenSeq[X]](data: GS[V], k: Int, trainDS: Seq[(ClusterID, V)]): GS[(ClusterID, V)] = {
		data.map( v => (knnPredict(v, k, trainDS), v) ).asInstanceOf[GS[(ClusterID, V)]]
	}
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