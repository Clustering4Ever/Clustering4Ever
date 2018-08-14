package clustering4ever.scala.clustering.kmeans

import scala.math.{min, max}
import scala.collection.{immutable, mutable}
import scala.util.Random
import clustering4ever.clustering.datasetstype.DataSetsTypes
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.math.distances.ContinuousDistances
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.SumArrays
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.RealClusterizable
import clustering4ever.scala.vectorizables.RealVectorizable
/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistances distance function
 **/
class KMeans[ID: Numeric, Obj](
	val data: immutable.Seq[RealClusterizable[ID, Obj]],
	var k: Int,
	var epsilon: Double,
	var iterMax: Int,
	var metric: ContinuousDistances = new Euclidean(true),
	var initializedCenters: mutable.HashMap[Int, immutable.Seq[Double]] = mutable.HashMap.empty[Int, immutable.Seq[Double]]
) extends ClusteringAlgorithms[Int, immutable.Seq[Double]]
{
	val realDS = data.map(_.vector)
	val dim = realDS.head.size

	/**
	 * Simplest centers initializations
	 * We search range for each dimension and take a random value between each range 
	 **/
	def initializationCenters() =
	{
		val (minv, maxv) = Stats.obtainMinAndMax(realDS)
		val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }.toSeq
		val centers = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
		centers
	}

	/**
	 * Run the K-Means
	 **/
	def run(): KMeansModel =
	{
		val centers = if( initializedCenters.isEmpty ) initializationCenters() else initializedCenters
		val centersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0) }

		def obtainNearestCenterID(v: immutable.Seq[Double]): ClusterID =
		{
			centers.minBy{ case (clusterID, center) => metric.d(center, v) }._1
		}

		/**
		 * Compute the similarity matrix and extract point which is the closest from all other point according to its dissimilarity measure
		 **/
		def obtainMedoid(arr: immutable.Seq[immutable.Seq[Double]]): immutable.Seq[Double] =
		{
			(for( v1 <- arr) yield (v1, (for( v2 <- arr ) yield metric.d(v1, v2)).sum / arr.size)).minBy(_._2)._1
		}
		/**
		 * Check if there are empty centers and remove them
		 **/
		def removeEmptyClusters(kCentersBeforeUpdate: mutable.HashMap[Int, immutable.Seq[Double]]) =
		{
			val emptyCenterIDs = centersCardinality.filter(_._2 == 0).map(_._1)
			centers --= emptyCenterIDs
			kCentersBeforeUpdate --= emptyCenterIDs
		}

		val zeroCenter = immutable.Seq.fill(dim)(0D)
		var cpt = 0
		var allCentersHaveConverged = false
		while( cpt < iterMax && ! allCentersHaveConverged )
		{
			// Allocation to nearest centroid
			val clusterized = realDS.map( v => (v, obtainNearestCenterID(v)) )
			val kCentersBeforeUpdate = centers.clone
			// Reinitialization of centers
			centers.foreach{ case (clusterID, center) => centers(clusterID) = zeroCenter }
			centersCardinality.foreach{ case (clusterID, _) => centersCardinality(clusterID) = 0 }

			if( metric.isInstanceOf[Euclidean] )
			{
				// Updatating Center
				clusterized.foreach{ case (v, clusterID) =>
				{
					centers(clusterID) = SumArrays.sumArraysNumerics[Double](centers(clusterID), v)
					centersCardinality(clusterID) += 1
				}}
				removeEmptyClusters(kCentersBeforeUpdate)
				// Update center vector
				centers.foreach{ case (clusterID, center) => centers(clusterID) = center.map(_ / centersCardinality(clusterID)) }
			}
			else
			{
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregates) =>
				{
					val cluster = aggregates.map{ case (vector, _) => vector }
					val centroid = obtainMedoid(cluster)
					centers(clusterID) = centroid
					centersCardinality(clusterID) += 1
				}}
				removeEmptyClusters(kCentersBeforeUpdate)
			}

			allCentersHaveConverged = kCentersBeforeUpdate.forall{ case (clusterID, previousCenter) => metric.d(previousCenter, centers(clusterID)) <= epsilon }
			cpt += 1
		}
		new KMeansModel(centers, metric)
	}
}

object KMeans extends DataSetsTypes[Int, immutable.Seq[Double]]
{
	/**
	 * Run the K-Means
	 **/
	def run[ID: Numeric, Obj](
		data: immutable.Seq[RealClusterizable[ID, Obj]],
		k: Int,
		epsilon: Double,
		iterMax: Int,
		metric: ContinuousDistances,
		initializedCenters: mutable.HashMap[Int, immutable.Seq[Double]] = mutable.HashMap.empty[Int, immutable.Seq[Double]]
		): KMeansModel =
	{
		val kMeans = new KMeans[ID, Obj](data, k, epsilon, iterMax, metric, initializedCenters)
		val kmeansModel = kMeans.run()
		kmeansModel
	}
}