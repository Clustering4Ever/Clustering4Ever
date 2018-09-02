package clustering4ever.scala.clustering

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.pow
import scala.collection.{immutable, mutable, parallel, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import clustering4ever.math.distances.Distance
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.{ClusterizableExt, Clusterizable}
import clustering4ever.clustering.{ClusteringAlgorithms, CommonPredictClusteringModel}
import clustering4ever.scala.measurableclass.BinaryScalarVector

abstract class KCommons[ID: Numeric, V, D <: Distance[V]](metric: D) extends ClusteringAlgorithms[ID]
{
	/**
	 * Check if there are empty centers and remove them
	 */
	protected def removeEmptyClusters(centers: mutable.HashMap[Int, V], kCentersBeforeUpdate: mutable.HashMap[Int, V], centersCardinality: mutable.HashMap[Int, Int]): Unit =
	{
		val emptyCenterIDs = centersCardinality.collect{ case (clusterID, cardinality) if( cardinality == 0 ) => clusterID }
		if( ! emptyCenterIDs.isEmpty )
		{
			centers --= emptyCenterIDs
			kCentersBeforeUpdate --= emptyCenterIDs
		}
	}
	/**
	 * Reinitialization of cardinalities
	 */
	protected def resetCentersCardinality(centersCardinality: mutable.HashMap[Int, Int]) = centersCardinality.foreach{ case (clusterID, _) => centersCardinality(clusterID) = 0 }
	/**
	 *
	 */
	protected def obtainNearestCenterID(v: V, centers: mutable.HashMap[Int, V]): ClusterID = centers.minBy{ case (clusterID, center) => metric.d(center, v) }._1
	/**
	 *
	 */
	protected def clusterizedAndSaveCenters(vectorizedDataset: GenSeq[V], centers: mutable.HashMap[Int, V]): (GenSeq[(V, Int)], mutable.HashMap[Int, V]) =
	{
		// Allocation to nearest centroid
		val clusterized = vectorizedDataset.map( v => (v, obtainNearestCenterID(v, centers)) )
		// Keep old position of centroids
		val kCentersBeforeUpdate = centers.clone
		(clusterized, kCentersBeforeUpdate)
	}
	
	/**
	 * Check if centers move enough
	 * @return true if every centers move less than epsilon
	 */
	protected def areCentersMovingEnough(kCentersBeforeUpdate: mutable.HashMap[Int, V], centers: mutable.HashMap[Int, V], epsilon: Double) = kCentersBeforeUpdate.forall{ case (clusterID, previousCenter) => metric.d(previousCenter, centers(clusterID)) <= epsilon }

	protected def removeEmptyClustersAndCheckIfallCentersHaveConverged(centers: mutable.HashMap[Int, V], kCentersBeforeUpdate: mutable.HashMap[Int, V], centersCardinality: mutable.HashMap[Int, Int], epsilon: Double): Boolean =
	{
		removeEmptyClusters(centers, kCentersBeforeUpdate, centersCardinality)
		areCentersMovingEnough(kCentersBeforeUpdate, centers, epsilon)
	}
	/**
	 * Compute the similarity matrix and extract point which is the closest from all other point according to its dissimilarity measure
	 */
	protected def obtainMedoid(gs: GenSeq[V]): V =
	{
		gs.minBy( v1 =>
		{
			var sum = 0D
			gs.foreach( v2 => sum += metric.d(v1, v2) )
			sum
		})
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
	protected def kmppInitialization[GS <: GenSeq[V]](vectorizedDataset: GS, k: Int): mutable.HashMap[Int, V] =
	{
		def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
		
		val centersBuff = mutable.ArrayBuffer(vectorizedDataset(Random.nextInt(vectorizedDataset.size)))

		(1 until k).foreach( i => centersBuff += Stats.obtainCenterFollowingWeightedDistribution[V](vectorizedDataset.map( v => (v, pow(metric.d(v, obtainNearestCenter(v, centersBuff)), 2)) ).toBuffer) )

		val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}

	/**
	 * Use a sweat Java library to do the job but requires k conversion in java.List of the datasets 
	 */
	protected def kmppInitializationJava[GS <: GenSeq[V]](vectorizedDataset: GS, k: Int): mutable.HashMap[Int, V] =
	{
		def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
		
		val centersBuff = mutable.ArrayBuffer(vectorizedDataset(Random.nextInt(vectorizedDataset.size)))
		
		val vectorizedDatasetList = vectorizedDataset.toList

		(1 until k).foreach{ i =>
			val prob = vectorizedDatasetList.map( v => new Pair(v, double2Double(pow(metric.d(v, obtainNearestCenter(v, centersBuff)), 2))) ).asJava
			val wDistribution = new EnumeratedDistribution(prob)
			centersBuff += wDistribution.sample
		}
		val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
}

abstract class KCommonsScala[
	ID: Numeric,
	V,
	D <: Distance[V],
	Cz <: Clusterizable[ID, V]
	](
	data: GenSeq[Cz],
	metric: D,
	k: Int,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommons[ID, V, D](metric)
{	
	val vectorizedDataset: GenSeq[V] = data.map(_.vector)
	val centers: mutable.HashMap[Int, V] = if( initializedCenters.isEmpty ) kmppInitialization(vectorizedDataset, k) else initializedCenters
	val centersCardinality: mutable.HashMap[Int, Int] = centers.map{ case (clusterID, _) => (clusterID, 0) }
	
	protected def clusterizedAndSaveCentersWithResetingCentersCardinalities(centers: mutable.HashMap[Int, V], centersCardinality: mutable.HashMap[Int, Int]): (GenSeq[(V, Int)], mutable.HashMap[Int, V]) = {
		resetCentersCardinality(centersCardinality)
		clusterizedAndSaveCenters(vectorizedDataset, centers)
	}
	/**
	 * Update Center and Cardinalities
	 */
	protected def updateCentersAndCardinalitiesCustom(clusterized: GenSeq[(V, Int)], centers: mutable.HashMap[Int, V], centersCardinality: mutable.HashMap[Int, Int]) = {		
		clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
			centers(clusterID) = obtainMedoid(aggregate.map(_._1))
			centersCardinality(clusterID) += aggregate.size
		}
	}
	protected def runKAlgorithmWithCustomMetric(maxIterations: Int, epsilon: Double) =
	{
		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < maxIterations && ! allCentersHaveConverged )
		{
			val (clusterized, kCentersBeforeUpdate) = clusterizedAndSaveCentersWithResetingCentersCardinalities(centers, centersCardinality)
			updateCentersAndCardinalitiesCustom(clusterized, centers, centersCardinality)
			allCentersHaveConverged = removeEmptyClustersAndCheckIfallCentersHaveConverged(centers, kCentersBeforeUpdate, centersCardinality, epsilon)
			cpt += 1
		}
	}
}

abstract class KCommonsVectors[
	ID: Numeric,
	N: Numeric,
	V <: Seq[N],
	D <: Distance[V],
	Cz <: Clusterizable[ID, V]
	](
	data: GenSeq[Cz],
	metric: D,
	k: Int,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsScala[ID, V, D, Cz](data, metric, k, initializedCenters)
{
	protected val dim = vectorizedDataset.head.size
}

abstract class KCommonsMixt[
	ID: Numeric,
	Vb <: Seq[Int],
	Vs <: Seq[Double],
	V <: BinaryScalarVector[Vb, Vs],
	D <: Distance[V],
	Cz <: Clusterizable[ID, V]
	](
	data: GenSeq[Cz],
	metric: D,
	k: Int,
	initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommonsScala[ID, V, D, Cz](data, metric, k, initializedCenters)
{
	protected val dimBinary = vectorizedDataset.head.binary.size
	protected val dimScalar = vectorizedDataset.head.scalar.size
}

abstract class KCommonsModel[
	ID: Numeric,
	V,
	D <: Distance[V],
	Cz <: ClusterizableExt[ID, V]
	](
	val centers: mutable.HashMap[Int, V],
	val metric: D
) extends CommonPredictClusteringModel[V, D]
{
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 **/
	def centerPredict(data: GenSeq[Cz])(implicit i: DummyImplicit): GenSeq[Cz] = data.map( rc => rc.setClusterID(centerPredict(rc.vector)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: GenSeq[Cz], k: Int, trainDS: Seq[(ClusterID, V)])(implicit i: DummyImplicit): GenSeq[Cz] = data.map( rc => rc.setClusterID(knnPredict(rc.vector, k, trainDS)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: GenSeq[Cz], k: Int, trainDS: Seq[Cz]): GenSeq[Cz] = knnPredict(data, k, trainDS.map( rc => (rc.clusterID, rc.vector) ))
}

/**
 * Object which regroup divers initializations methods
 */
object KCommonsInitializations
{
	/**
	 * Simplest centers initialization
	 * We search range for each dimension and take a random value between each range 
	 */
	def naiveInitializationReal[GS <: GenSeq[mutable.ArrayBuffer[Double]]](vectorizedDataset: GS, k: Int) =
	{
		val (minv, maxv) = Stats.obtainMinAndMax(vectorizedDataset)
		val ranges = mutable.ArrayBuffer(minv.zip(maxv).map{ case (min, max) => (max - min, min) }:_*)
		val centers: mutable.HashMap[Int, mutable.ArrayBuffer[Double]] = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
		centers
	}
	/**
	 * Simplest centers initialization which generate random binary vectors 
	 */
	def naiveInitializationBinary(dim: Int, k: Int) = mutable.HashMap((0 until k).map( clusterID => (clusterID, mutable.ArrayBuffer.fill(dim)(Random.nextInt(2))) ):_*) 

	/**
	 * Simplest centroids initializations
	 * We search range for each dimension and take a random value between each range for scalar data and take a random {0, 1} for binary data
	 **/
	def naiveInitializationMixt(vectorizedDataset: GenSeq[BinaryScalarVector[mutable.ArrayBuffer[Int], mutable.ArrayBuffer[Double]]], k: Int): mutable.HashMap[Int, BinaryScalarVector[mutable.ArrayBuffer[Int], mutable.ArrayBuffer[Double]]] =
	{
		val realPart = vectorizedDataset.map(_.scalar)
		val h = vectorizedDataset.head
		val dimBinary = h.binary.size
		val dimScalar = h.scalar.size
		val vectorRange = (0 until dimScalar).toBuffer

		val realPartCenters = naiveInitializationReal(realPart, k)
		val binaryPartCenters = naiveInitializationBinary(dimBinary, k)

		binaryPartCenters.zip(realPartCenters).map{ case ((clusterID, binary), (_, scalar)) => (clusterID, new BinaryScalarVector[mutable.ArrayBuffer[Int], mutable.ArrayBuffer[Double]](binary, scalar)) }
	}
}