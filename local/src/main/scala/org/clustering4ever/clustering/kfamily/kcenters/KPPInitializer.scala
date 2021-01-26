package org.clustering4ever.clustering.kfamily.kcenters

/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances.Distance
import org.clustering4ever.math.Stats
import org.clustering4ever.roottraits.{Clusterizable, GVector}

import scala.collection.{GenSeq, immutable, mutable}
import scala.language.higherKinds
import scala.util.Random
/**
 * Kmeans++ initialization
 * <h2>References</h2>
 * <ol>
 * <li> Tapas Kanungo, David M. Mount, Nathan S. Netanyahu, Christine D. Piatko, Ruth Silverman, and Angela Y. Wu. An Efficient k-Means Clustering Algorithm: Analysis and Implementation. IEEE TRANS. PAMI, 2002.</li>
 * <li> D. Arthur and S. Vassilvitskii. "K-means++: the advantages of careful seeding". ACM-SIAM symposium on Discrete algorithms, 1027-1035, 2007.</li>
 * <li> Anna D. Peterson, Arka P. Ghosh and Ranjan Maitra. A systematic evaluation of different methods for initializing the K-means clustering algorithm. 2010.</li>
 * </ol>
 */
object KPPInitializer extends Serializable {
	/**
	 * Kmeans++ initialization
	 * <h2>References</h2>
	 * <ol>
	 * <li> Tapas Kanungo, David M. Mount, Nathan S. Netanyahu, Christine D. Piatko, Ruth Silverman, and Angela Y. Wu. An Efficient k-Means Clustering Algorithm: Analysis and Implementation. IEEE TRANS. PAMI, 2002.</li>
	 * <li> D. Arthur and S. Vassilvitskii. "K-means++: the advantages of careful seeding". ACM-SIAM symposium on Discrete algorithms, 1027-1035, 2007.</li>
	 * <li> Anna D. Peterson, Arka P. Ghosh and Ranjan Maitra. A systematic evaluation of different methods for initializing the K-means clustering algorithm. 2010.</li>
	 * </ol>
	 */
	final def kppInit[
		O,
		V <: GVector[V],
		Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
		D <: Distance[V]
	](data: GenSeq[Cz[O, V]], metric: D, k: Int): immutable.HashMap[Int, V] = {

		val centers = mutable.ArrayBuffer(data(Random.nextInt(data.size)).v)

		def obtainNearestCenter(v: V): V = centers.minBy(metric.d(_, v))

		@annotation.tailrec
		def go(i: Int): Unit = {
			val preprocessed = data.map{ cz =>
				val toPow2 = metric.d(cz.v, obtainNearestCenter(cz.v))
				(cz.v, toPow2 * toPow2)
			}
			val phi = preprocessed.aggregate(0D)((agg, e) => agg + e._2, _ + _)
			val probabilities = preprocessed.map{ case (v, toPow2) => (v, toPow2 / phi) }.seq
			val shuffled = Random.shuffle(probabilities)
			centers += Stats.obtainMedianFollowingWeightedDistribution[V](shuffled)
			if(i < k - 2) go(i + 1)
		}

		go(0)
		
		immutable.HashMap(centers.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)

	}
}