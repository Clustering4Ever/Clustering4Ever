package clustering4ever.scala.clustering.kmeans

import _root_.clustering4ever.clustering.datasetstype.ClusteringTypes
import _root_.clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.util.SumArrays
import _root_.scala.math.{min, max}
import _root_.scala.collection.{immutable, mutable}
import _root_.scala.util.Random

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
	data: Array[(Int, Array[Double])],
	var k: Int,
	var epsilon: Double,
	var iterMax: Int,
	var metric: ContinuousDistances
) extends ClusteringAlgorithms[Int, Double, Array[(Int, (Int, Array[Double]))]]
{
	val dim = data.head._2.size

	/**
	 * Simplest centroids initializations
	 * We search range for each dimension and take a random value between each range 
	 **/
	def initializationCentroids =
	{
		val vectorRange = (0 until dim).toArray

		def obtainMinMax(idx: Int, vminMax1: (Array[Double], Array[Double]), vminMax2: (Array[Double], Array[Double])) =
		{
			(
				min(vminMax1._1(idx), vminMax2._1(idx)),
				max(vminMax1._2(idx), vminMax2._2(idx))
			)
		}

		val (minv, maxv) = data.map{ case (_, v) => (v, v) }.reduce( (minMaxa, minMaxb) =>
		{
			val minAndMax = for( i <- vectorRange ) yield( obtainMinMax(i, minMaxa, minMaxb) )
			minAndMax.unzip
		})

		val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }
		val modes = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
		modes
	}

	/**
	 * Run the K-Means
	 **/
	def run(): ClusterizedData =
	{
		val kmodes = initializationCentroids
		val kmodesCardinalities = kmodes.map{ case (clusterID, _) => (clusterID, 0) }

		def obtainNearestModID(v: Array[Double]): ClusterID = kmodes.toArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.sortBy(_._2).head._1

		val zeroMod = Array.fill(dim)(0D)
		var cpt = 0
		var allModsHaveConverged = false
		while( cpt < iterMax && ! allModsHaveConverged )
		{
			// Allocation to modes
			val clusterized = data.map{ case (id, v) => (id, v, obtainNearestModID(v)) }

			val kModesBeforeUpdate = kmodes.clone

			// Reinitialization of modes
			kmodes.foreach{ case (clusterID, mod) => kmodes(clusterID) = zeroMod }
			kmodesCardinalities.foreach{ case (clusterID, _) => kmodesCardinalities(clusterID) = 0 }

			// Updatating Modes
			clusterized.foreach{ case (_, v, clusterID) =>
			{
				kmodes(clusterID) = SumArrays.sumArraysNumerics(kmodes(clusterID), v)
				kmodesCardinalities(clusterID) += 1
			}}

			kmodes.foreach{ case (clusterID, mod) => kmodes(clusterID) = mod.map(_ / kmodesCardinalities(clusterID)) }

			allModsHaveConverged = kModesBeforeUpdate.forall{ case (clusterID, previousMod) => metric.d(previousMod, kmodes(clusterID)) <= epsilon }

			cpt += 1
		}

		val finalClustering = data.map{ case (id, v) =>
		{
			val clusterID = obtainNearestModID(v)
			(clusterID, (id, v))
		}}
		finalClustering
	}
}

object KMeans extends ClusteringTypes[Int, Double, Array[(Int, (Int, Array[Double]))]]
{
	/**
	 * Run the K-Means
	 **/
	def run(data: Array[(ID, Vector)], k: Int, epsilon: Double, iterMax: Int, metric: ContinuousDistances): ClusterizedData =
	{
		val kmodes = new KMeans(data, k, epsilon, iterMax, metric)
		val kmodesClusterized = kmodes.run()
		kmodesClusterized
	}
}