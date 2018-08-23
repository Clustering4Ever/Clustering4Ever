package clustering4ever.scala.clustering.kprotoypes

import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.{min, max, pow}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import clustering4ever.clustering.datasetstype.DataSetsTypes
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.math.distances.mixt.HammingAndEuclidean
import clustering4ever.util.SumArrays
import clustering4ever.math.distances.{MixtDistance, MixtDistanceClusterizable}
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.ClusterizableM

/**
 * @author Beck Gaël
 * The famous K-Prototypes using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistance distance function
 **/
class KPrototypes[ID: Numeric, Obj](
	data: GenSeq[ClusterizableM[ID, Obj]],
	var k: Int,
	var epsilon: Double,
	var iterMax: Int,
	var metric: MixtDistance
) extends ClusteringAlgorithms[ID]
{
	private[this] val mixtDS = data.map{ clusterizable =>
	{
		val (binaryV, realV) = clusterizable.vector
		new BinaryScalarVector(binaryV, realV)
	}}
	private[this] val dimBinary = mixtDS.head.scalar.size
	private[this] val dimScalar = mixtDS.head.binary.size
	/**
	 * Simplest centroids initializations
	 * We search range for each dimension and take a random value between each range for scalar data and take a random {0, 1} for binary data
	 **/
	def initialization(): mutable.HashMap[Int, BinaryScalarVector] =
	{
		val vectorRange = (0 until dimScalar).toVector
		val numberClustersRange = (0 until k).toSeq

		val binaryModes = numberClustersRange.map( clusterID => (clusterID, Vector.fill(dimBinary)(Random.nextInt(2))) )

		val (minv, maxv) = mixtDS.map( v =>
		{
			val vector = v.scalar.toVector
			(vector, vector)
		}).reduce( (minMaxa, minMaxb) => vectorRange.map( i => Stats.obtainIthMinMax(i, minMaxa, minMaxb) ).unzip )

		val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }
		val scalarCenters = numberClustersRange.map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) )
		
		mutable.HashMap(binaryModes.zip(scalarCenters).map{ case ((clusterID, binaryVector), (_, scalarVector)) => (clusterID, new BinaryScalarVector(binaryVector, scalarVector)) }:_*)
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
	def kmppInitialization() =
	{
		val firstCenter = mixtDS(Random.nextInt(mixtDS.size))
		var choosenCenter = firstCenter
		val centersKminus1: immutable.IndexedSeq[BinaryScalarVector] = for( i <- 1 until k ) yield {
			val prob = mixtDS.map( v => new Pair(v, double2Double(pow(metric.d(v, choosenCenter), 2))) ).toList.asJava
			val wDistribution = new EnumeratedDistribution(prob)
			choosenCenter = wDistribution.sample
			choosenCenter
		}
		val center: mutable.HashMap[Int, BinaryScalarVector] = mutable.HashMap((centersKminus1 :+ firstCenter).zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		center
	}
	/**
	 * Run the K-Means
	 **/
	def run(): KPrototypesModel =
	{
		val centers = initialization()
		val centersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0) }

		def obtainNearestModID(v: BinaryScalarVector): ClusterID = centers.minBy{ case(clusterID, mode) => metric.d(mode, v) }._1

		/**
		 * Check if there are empty centers and remove them
		 **/
		def removeEmptyClusters(kCentersBeforeUpdate: mutable.HashMap[Int, BinaryScalarVector]) =
		{
			// Check if there are empty centers and remove them
			val emptyCenterIDs = centersCardinality.filter(_._2 == 0).map(_._1)
			centers --= emptyCenterIDs
			kCentersBeforeUpdate --= emptyCenterIDs
		}


		val zeroMode = new BinaryScalarVector(Vector.fill(dimBinary)(0), Vector.fill(dimScalar)(0D))
		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < iterMax && ! allCentersHaveConverged )
		{
			// Allocation to nearest centroid
			val clusterized = mixtDS.map( v => (v, obtainNearestModID(v)) )

			val kCentersBeforeUpdate = centers.clone

			// Reinitialization of centers
			centers.foreach{ case (clusterID, mode) => centers(clusterID) = zeroMode }
			centersCardinality.foreach{ case (clusterID, _) => centersCardinality(clusterID) = 0 }

			if( metric.isInstanceOf[HammingAndEuclidean] )
			{
				// Updatating Modes
				clusterized.foreach{ case (v, clusterID) =>
				{
					centers(clusterID) =
					{
						new BinaryScalarVector(
							SumArrays.sumArraysNumerics[Int](centers(clusterID).binary, v.binary),
							SumArrays.sumArraysNumerics[Double](centers(clusterID).scalar, v.scalar)
						)
					}
					centersCardinality(clusterID) += 1
				}}
				// Update center vector
				centers.foreach{ case (clusterID, mode) => centers(clusterID) =
				{
					new BinaryScalarVector(
						mode.binary.map( v => if( v * 2 > centersCardinality(clusterID) ) 1 else 0 ),
						mode.scalar.map(_ / centersCardinality(clusterID))
					)
				}}
				removeEmptyClusters(kCentersBeforeUpdate)
			}
			else println("We have a bit of time before thinking of mixt data with custom distances")

			allCentersHaveConverged = kCentersBeforeUpdate.forall{ case (clusterID, previousMod) => metric.d(previousMod, centers(clusterID)) <= epsilon }
			cpt += 1
		}
		new KPrototypesModel(centers, metric)
	}
}

object KPrototypes
{
	/**
	 * Run the K-Protypes
	 **/
	def run[ID: Numeric, Obj](data: GenSeq[ClusterizableM[ID, Obj]], k: Int, epsilon: Double, iterMax: Int, metric: MixtDistance): KPrototypesModel =
	{
		val kPrototypes = new KPrototypes(data, k, epsilon, iterMax, metric)
		val kPrototypesModel = kPrototypes.run()
		kPrototypesModel
	}
}