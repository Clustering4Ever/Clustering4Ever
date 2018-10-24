package clustering4ever.scala.clustering
/**
 * @author Beck Gaël
 */
import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.pow
import scala.collection.{GenSeq, mutable}
import scala.util.Random
import clustering4ever.math.distances.Distance
import clustering4ever.stats.Stats
import clustering4ever.scala.measurableclass.{BinaryScalarVector, SimpleBinaryScalarVector}
/**
 * This object gather different initialization methods for K-Means, K-Modes, K-Prototypes
 */
object KppInitialization {
	/**
	 * Simplest centers initialization
	 * We search range for each dimension and take a random value between each range 
	 */
	def naiveInitializationReal[GS <: GenSeq[mutable.ArrayBuffer[Double]]](realDS: GS, k: Int): mutable.HashMap[Int, mutable.ArrayBuffer[Double]] =	{
		val (minv, maxv) = Stats.obtainMinAndMax(realDS)
		val ranges = mutable.ArrayBuffer(minv.zip(maxv).map{ case (min, max) => (max - min, min) }:_*)
		val centers = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
		centers
	}
	/**
	 * Simplest centers initialization which generate random binary vectors 
	 */
	def naiveInitializationBinary(dim: Int, k: Int): mutable.HashMap[Int, mutable.ArrayBuffer[Int]] = mutable.HashMap((0 until k).map( clusterID => (clusterID, mutable.ArrayBuffer.fill(dim)(Random.nextInt(2))) ):_*) 
	/**
	 * Simplest centroids initializations
	 * We search range for each dimension and take a random value between each range for scalar data and take a random {0, 1} for binary data
	 */
	def naiveInitializationMixt(vectorizedDataset: GenSeq[BinaryScalarVector[mutable.ArrayBuffer[Int], mutable.ArrayBuffer[Double]]], k: Int): mutable.HashMap[Int, BinaryScalarVector[mutable.ArrayBuffer[Int], mutable.ArrayBuffer[Double]]] = {
		val realPart = vectorizedDataset.map(_.scalar)
		val h = vectorizedDataset.head
		val dimBinary = h.binary.size
		val dimScalar = h.scalar.size
		val vectorRange = (0 until dimScalar).toBuffer

		val realPartCenters = naiveInitializationReal(realPart, k)
		val binaryPartCenters = naiveInitializationBinary(dimBinary, k)

		binaryPartCenters.zip(realPartCenters).map{ case ((clusterID, binary), (_, scalar)) => (clusterID, new SimpleBinaryScalarVector[mutable.ArrayBuffer[Int], mutable.ArrayBuffer[Double]](binary, scalar)) }
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
	def kmppInitialization[V, GS <: GenSeq[V]](vectorizedDataset: GS, k: Int, metric: Distance[V]): mutable.HashMap[Int, V] = {
		def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
		
		val centersBuff = mutable.ArrayBuffer(vectorizedDataset(Random.nextInt(vectorizedDataset.size)))

		(1 until k).foreach( i => centersBuff += Stats.obtainCenterFollowingWeightedDistribution[V](vectorizedDataset.map{ v =>
			val toPow2 = metric.d(v, obtainNearestCenter(v, centersBuff))
			(v, toPow2 * toPow2)
		}.toBuffer) )

		val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
	/**
	 * Use a sweat Java library to do the job but requires k conversion in java.List of the datasets 
	 */
	def kmppInitializationJava[V, GS <: GenSeq[V]](vectorizedDataset: GS, k: Int, metric: Distance[V]): mutable.HashMap[Int, V] = {
		
		def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
		
		val centersBuff = mutable.ArrayBuffer(vectorizedDataset(Random.nextInt(vectorizedDataset.size)))
		
		val vectorizedDatasetList = vectorizedDataset.toList

		(1 until k).foreach{ i =>
			val prob = vectorizedDatasetList.map( v => new Pair(v, double2Double({
				val toPow2 = metric.d(v, obtainNearestCenter(v, centersBuff))
				toPow2 * toPow2
			})) ).asJava
			val wDistribution = new EnumeratedDistribution(prob)
			centersBuff += wDistribution.sample
		}
		val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
}
