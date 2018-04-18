package clustering4ever.scala.clustering.kmeans

import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.math.distances.scalar.Euclidean
import _root_.clustering4ever.util.SumArrays
import _root_.scala.math.{min, max}
import _root_.scala.collection.{immutable, mutable}
import _root_.scala.util.Random
import _root_.clustering4ever.stats.Stats

/**
 * @author Beck Gaël
 * The famous K-Means using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistances distance function
 **/
class KMeans(
	data: Seq[Array[Double]],
	var k: Int,
	var epsilon: Double,
	var iterMax: Int,
	var metric: ContinuousDistances
) extends ClusteringAlgorithms[Int, Array[Double]]
{
	val dim = data.head.size
	/**
	 * Simplest centers initializations
	 * We search range for each dimension and take a random value between each range 
	 **/
	def initializationCenters =
	{
		val (minv, maxv) = Stats.obtainMinAndMax(data)
		val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }
		val centers = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
		centers
	}

	/**
	 * Run the K-Means
	 **/
	def run(): KMeansModel =
	{
		val centers = initializationCenters
		val clustersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0) }

		def obtainNearestModID(v: Array[Double]): ClusterID =
		{
			centers.toArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.minBy(_._2)._1
		}

		/**
		 * Compute the similarity matrix and extract point which is the closest from all other point according to its dissimilarity measure
		 **/
		def obtainMedoid(arr: Seq[Array[Double]]): Array[Double] =
		{
			(for( v1 <- arr) yield( (v1, (for( v2 <- arr ) yield(metric.d(v1, v2))).sum / arr.size) )).sortBy(_._2).head._1
		}

		val zeroMod = Array.fill(dim)(0D)
		var cpt = 0
		var allModsHaveConverged = false
		while( cpt < iterMax && ! allModsHaveConverged )
		{
			// Allocation to nearest centroid
			val clusterized = data.map( v => (v, obtainNearestModID(v)) )

			val kModesBeforeUpdate = centers.clone

			// Reinitialization of centers
			centers.foreach{ case (clusterID, mod) => centers(clusterID) = zeroMod }
			clustersCardinality.foreach{ case (clusterID, _) => clustersCardinality(clusterID) = 0 }

			if( metric.isInstanceOf[Euclidean] )
			{
				// Updatating Modes
				clusterized.foreach{ case (v, clusterID) =>
				{
					centers(clusterID) = SumArrays.sumArraysNumerics(centers(clusterID), v)
					clustersCardinality(clusterID) += 1
				}}

				centers.foreach{ case (clusterID, mod) => centers(clusterID) = mod.map(_ / clustersCardinality(clusterID)) }
			}
			else
			{
				clusterized.groupBy{ case (_, clusterID) => clusterID }.foreach{ case (clusterID, aggregates) =>
				{
					val cluster = aggregates.map{ case (vector, _) => vector }
					val centroid = obtainMedoid(cluster)
					centers(clusterID) = centroid
				}}
			}

			allModsHaveConverged = kModesBeforeUpdate.forall{ case (clusterID, previousMod) => metric.d(previousMod, centers(clusterID)) <= epsilon }

			cpt += 1
		}
		new KMeansModel(centers, clustersCardinality, metric)
	}
}

object KMeans extends DataSetsTypes[Int, Array[Double]]
{
	/**
	 * Run the K-Means
	 **/
	def run(data: Seq[Vector], k: Int, epsilon: Double, iterMax: Int, metric: ContinuousDistances): KMeansModel =
	{
		val kMeans = new KMeans(data, k, epsilon, iterMax, metric)
		val kmeansModel = kMeans.run()
		kmeansModel
	}
}