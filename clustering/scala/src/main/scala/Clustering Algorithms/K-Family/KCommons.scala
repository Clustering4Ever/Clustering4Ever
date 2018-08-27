package clustering4ever.scala.clustering

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.pow
import scala.collection.{immutable, mutable, GenSeq, parallel}
import scala.util.Random
import scala.reflect.ClassTag
import clustering4ever.math.distances.Distance
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.{ClusterizableExt, Clusterizable}
import clustering4ever.clustering.{ClusteringAlgorithms, CommonPredictClusteringModel}
import clustering4ever.scala.measurableclass.BinaryScalarVector

abstract class KCommons[ID: Numeric, V, D <: Distance[V]](var metric: D) extends ClusteringAlgorithms[ID]
{
	/**
	 * Check if there are empty centers and remove them
	 */
	protected def removeEmptyClusters(centers: mutable.HashMap[Int, V], kCentersBeforeUpdate: mutable.HashMap[Int, V], centersCardinality: mutable.HashMap[Int, Int]): Unit =
	{
		val emptyCenterIDs = centersCardinality.filter(_._2 == 0).map(_._1)
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
	protected def clusterizedAndSaveCenters(realDS: parallel.ParSeq[V], centers: mutable.HashMap[Int, V]): (parallel.ParSeq[(V, Int)], mutable.HashMap[Int, V]) =
	{
		// Allocation to nearest centroid
		val clusterized = realDS.map( v => (v, obtainNearestCenterID(v, centers)) )
		// Keep old position of centroids
		val kCentersBeforeUpdate = centers.clone
		(clusterized, kCentersBeforeUpdate)
	}
	/**
	 * Check if centers move enough
	 * @return true if every centers move less than epsilon
	 */
	protected def areCentersMovingEnough(kCentersBeforeUpdate: mutable.HashMap[Int, V], centers: mutable.HashMap[Int, V], epsilon: Double) = kCentersBeforeUpdate.forall{ case (clusterID, previousCenter) => metric.d(previousCenter, centers(clusterID)) <= epsilon }
	/**
	 * Simplest centers initialization
	 * We search range for each dimension and take a random value between each range 
	 */
	protected def naiveInitializationReal[S <: immutable.Seq[Double], GS <: GenSeq[S]](realDS: GS, k: Int) =
	{
		val (minv, maxv) = Stats.obtainMinAndMax(realDS)
		val ranges = immutable.Seq(minv.zip(maxv).map{ case (min, max) => (max - min, min) }:_*)
		val centers: mutable.HashMap[Int, immutable.Seq[Double]] = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
		centers
	}
	/**
	 * Simplest centers initialization which generate random binary vectors 
	 */
	protected def naiveInitializationBinary(dim: Int, k: Int) = mutable.HashMap((0 until k).map( clusterID => (clusterID, immutable.Seq.fill(dim)(Random.nextInt(2))) ):_*) 
	/**
	 * Kmeans++ initialization
	 * <h2>References</h2>
	 * <ol>
	 * <li> Tapas Kanungo, David M. Mount, Nathan S. Netanyahu, Christine D. Piatko, Ruth Silverman, and Angela Y. Wu. An Efficient k-Means Clustering Algorithm: Analysis and Implementation. IEEE TRANS. PAMI, 2002.</li>
	 * <li> D. Arthur and S. Vassilvitskii. "K-means++: the advantages of careful seeding". ACM-SIAM symposium on Discrete algorithms, 1027-1035, 2007.</li>
	 * <li> Anna D. Peterson, Arka P. Ghosh and Ranjan Maitra. A systematic evaluation of different methods for initializing the K-means clustering algorithm. 2010.</li>
	 * </ol>
	 */
	protected def kmppInitialization[GS <: GenSeq[V]](realDS: GS, k: Int): mutable.HashMap[Int, V] =
	{
		def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
		
		val centersBuff = mutable.ArrayBuffer(realDS(Random.nextInt(realDS.size)))
		
		for( i <- 1 until k ) {
			val prob = realDS.map( v => new Pair(v, double2Double(pow(metric.d(v, obtainNearestCenter(v, centersBuff)), 2))) ).toList.asJava
			val wDistribution = new EnumeratedDistribution(prob) 
			centersBuff += wDistribution.sample
		}
		val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
	/**
	 * Compute the similarity matrix and extract point which is the closest from all other point according to its dissimilarity measure
	 */
	private def obtainMedoid(gs: GenSeq[V]): V = gs.minBy( v1 => gs.map( v2 => metric.d(v1, v2) ).sum )
	/**
	 * Update Center and Cardinalities
	 */
	protected def updateCentersAndCardinalitiesCustom(clusterized: parallel.ParSeq[(V, Int)], centers: mutable.HashMap[Int, V], centersCardinality: mutable.HashMap[Int, Int]) =
	{		
		clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregate) =>
		{
			val mode = obtainMedoid(aggregate.map(_._1))
			centers(clusterID) = mode
			centersCardinality(clusterID) += 1
		}}
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
	var k: Int,
	var initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V]
) extends KCommons[ID, V, D](metric)
{
	val vectorizedDataset: parallel.ParSeq[V] = data.par.map(_.vector)
	val centers: mutable.HashMap[Int, V] = if( initializedCenters.isEmpty ) kmppInitialization[parallel.ParSeq[V]](vectorizedDataset, k) else initializedCenters
	val centersCardinality: mutable.HashMap[Int, Int] = centers.map{ case (clusterID, _) => (clusterID, 0) }
}

abstract class KCommonsVectors[
	ID: Numeric,
	N: Numeric,
	V <: immutable.Seq[N],
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
	Vb <: immutable.Seq[Int],
	Vs <: immutable.Seq[Double],
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

	/**
	 * Simplest centroids initializations
	 * We search range for each dimension and take a random value between each range for scalar data and take a random {0, 1} for binary data
	 **/
	def naiveInitializationMixt(): mutable.HashMap[Int, BinaryScalarVector[Vb, Vs]] =
	{
		val vectorRange = (0 until dimScalar).toVector
		val numberClustersRange = (0 until k).toSeq

		val binaryModes = numberClustersRange.map( clusterID => (clusterID, immutable.Seq.fill(dimBinary)(Random.nextInt(2)).asInstanceOf[Vb]) )

		val (minv, maxv) = vectorizedDataset.map( v =>
		{
			val vector = v.scalar.toVector
			(vector, vector)
		}).reduce( (minMaxa, minMaxb) => vectorRange.map( i => Stats.obtainIthMinMax(i, minMaxa, minMaxb) ).unzip )

		val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }.toSeq
		val scalarCenters = numberClustersRange.map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }.asInstanceOf[Vs]) )
		
		mutable.HashMap(binaryModes.zip(scalarCenters).map{ case ((clusterID, binaryVector), (_, scalarVector)) => (clusterID, new BinaryScalarVector[Vb, Vs](binaryVector, scalarVector)) }:_*)
	}
}

abstract class KCommonsModel[
	ID: Numeric,
	T: Numeric,
	V <: immutable.Seq[T] : ClassTag,
	D <: Distance[V],
	Cz <: ClusterizableExt[ID, T, V]
	](
	val centers: mutable.HashMap[Int, V],
	val metric: D
) extends CommonPredictClusteringModel[V, D]
{
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input GenSeq with labels obtain via centerPredict method
	 **/
	def centerPredict(data: GenSeq[Cz])(implicit i: DummyImplicit): GenSeq[Cz] = data.map( rc => rc.setClusterID(centerPredict(rc.vector)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input GenSeq with labels obtain via knnPredict method
	 */
	def knnPredict(data: GenSeq[Cz], k: Int, trainDS: Seq[(ClusterID, V)])(implicit i: DummyImplicit): GenSeq[Cz] = data.map( rc => rc.setClusterID(knnPredict(rc.vector, k, trainDS)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input GenSeq with labels obtain via knnPredict method
	 */
	def knnPredict(data: GenSeq[Cz], k: Int, trainDS: Seq[Cz]): GenSeq[Cz] = knnPredict(data, k, trainDS.map( rc => (rc.clusterID, rc.vector) ))
}