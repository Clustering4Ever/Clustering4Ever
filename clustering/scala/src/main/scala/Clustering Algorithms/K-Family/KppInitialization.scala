package clustering4ever.scala.clustering

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.pow
import scala.collection.{immutable, mutable}
import scala.util.Random
import clustering4ever.math.distances.Distance
import clustering4ever.stats.Stats

object KppInitialization
{
	/**
	 * Simplest centers initialization
	 * We search range for each dimension and take a random value between each range 
	 */
	def naiveInitialization[S <: immutable.Seq[Double], GS <: Seq[S]](realDS: GS, k: Int) =
	{
		val (minv, maxv) = Stats.obtainMinAndMax(realDS)
		val ranges = immutable.Seq(minv.zip(maxv).map{ case (min, max) => (max - min, min) }:_*)
		val centers: mutable.HashMap[Int, immutable.Seq[Double]] = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
		centers
	}
	/**
	 * Kmeans++ initialization
	 * <h2>References</h2>
	 * <ol>
	 * <li> Tapas Kanungo, David M. Mount, Nathan S. Netanyahu, Christine D. Piatko, Ruth Silverman, and Angela Y. Wu. An Efficient k-Means Clustering Algorithm: Analysis and Implementation. IEEE TRANS. PAMI, 2002.</li>
	 * <li> D. Arthur and S. Vassilvitskii. "K-means++: the advantages of careful seeding". ACM-SIAM symposium on Discrete algorithms, 1027-1035, 2007.</li>
	 * <li> Anna D. Peterson, Arka P. Ghosh and Ranjan Maitra. A systematic evaluation of different methods for initializing the K-means clustering algorithm. 2010.</li>
	 * </ol>
	 */
	def kmppInitialization[T: Numeric, S <: immutable.Seq[T], GS <: Seq[S]](realDS: GS, k: Int, metric: Distance[S]): mutable.HashMap[Int, S] =
	{
		def obtainNearestCenter(v: S, centers: mutable.ArrayBuffer[S]): S = centers.minBy(metric.d(_, v))
		
		val centersBuff = mutable.ArrayBuffer(realDS(Random.nextInt(realDS.size)))
		
		for( i <- 1 until k ) {
			val prob = realDS.map( v => new Pair(v, double2Double(pow(metric.d(v, obtainNearestCenter(v, centersBuff)), 2))) ).toList.asJava
			val wDistribution = new EnumeratedDistribution(prob) 
			centersBuff += wDistribution.sample
		}
		val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
}