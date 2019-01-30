package org.clustering4ever.clustering.kcenters.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{mutable, immutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import org.clustering4ever.stats.Stats
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.GVector
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
	def kppInit[
		ID,
		O,
		V <: GVector[V],
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
		D <: Distance[V]
	](data: GenSeq[Cz[ID, O, V]], metric: D, k: Int): immutable.HashMap[Int, V] = {

		// def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
		def obtainNearestCenter(v: V, centers: immutable.Vector[V]): V = centers.minBy(metric.d(_, v))

		@annotation.tailrec
		def go(i: Int, centers: immutable.Vector[V]): immutable.Vector[V] = {
			val updatedCenters = centers :+ Stats.obtainCenterFollowingWeightedDistribution[V]{
				data.map{ cz =>
					val toPow2 = metric.d(cz.v, obtainNearestCenter(cz.v, centers))
					(cz.v, toPow2 * toPow2)
				}.seq
			}
			if(i < k - 2) go(i + 1, updatedCenters)
			else updatedCenters
		}

		
		immutable.HashMap(go(0, immutable.Vector(data(Random.nextInt(data.size)).v)).zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)

	}
}