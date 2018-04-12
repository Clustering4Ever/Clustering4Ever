package clustering4ever.scala.clustering.gaussianmixture

import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.util.SumArrays
import _root_.scala.math.{min, max, sqrt, pow}
import _root_.scala.collection.{immutable, mutable}
import _root_.scala.util.Random
import _root_.clustering4ever.stats.Stats
import _root_.clustering4ever.scala.kernels.Kernels
import _root_.clustering4ever.math.distances.scalar.Euclidean

/**
 * @author Beck Gaël
 * Gaussian Mixtures using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistances distance function
 **/
class GaussianMixture(
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
	def initializationCentroids(): mutable.HashMap[Int, Array[Double]] =
	{
		val vectorRange = (0 until dim).toArray

		def obtainMinMax(idx: Int, vminMax1: (Array[Double], Array[Double]), vminMax2: (Array[Double], Array[Double])) =
		{
			(
				min(vminMax1._1(idx), vminMax2._1(idx)),
				max(vminMax1._2(idx), vminMax2._2(idx))
			)
		}

		val (minv, maxv) = data.map( v => (v, v) ).reduce( (minMaxa, minMaxb) =>
		{
			val minAndMax = for( i <- vectorRange ) yield obtainMinMax(i, minMaxa, minMaxb)
			minAndMax.unzip
		})

		val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }
		val centers = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
		centers
	}

	/**
	 * Run the Gaussian Mixture
	 **/
	def run(): GaussianMixtureModel =
	{
		val centers = initializationCentroids()
		val centersAsArray = centers.toArray
		val clustersCardinality = centers.map{ case (clusterID, _) => (clusterID, 0) }

		def obtainNearestModID(v: Array[Double]): ClusterID =
		{
			centersAsArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.sortBy(_._2).head._1
		}

		// Allocation to the nearest centroid
		val clusterized = data.map( v => (obtainNearestModID(v), v) )
	
		val gaussianLawFeatures = clusterized.groupBy(_._1).map{ case (clusterID, aggregates) =>
		{
			val vectors = aggregates.map(_._2).toArray
			val meanC = Stats.mean(vectors)
			val sdC = Stats.sd(vectors, meanC)
			(clusterID, (meanC, sdC))
		}}.toArray.sortBy(_._1)

		val checkingMeans = gaussianLawFeatures.map(_._2._1)
		val zeroMod = Array.fill(dim)(0D)
		var cpt = 0
		var allModsHaveConverged = false
		val πks = gaussianLawFeatures.map( x => 1D / k )
		while( cpt < iterMax && ! allModsHaveConverged )
		{
			val gammas = data.map( v => (v, Stats.obtainGammaByCluster(v, gaussianLawFeatures, πks)) )

			val gammasSums = SumArrays.sumColumnArrays(gammas.map(_._2.map(_._2)))

			val μs = gammas.map{ case (v, gammaByCluster) => gammaByCluster.map{ case (clusterID, coef) => v.map(_ * coef) }}.reduce(SumArrays.reduceMultipleVectorsMatrice).zip(gammasSums).map{ case (v, gammaSum) => v.map(_ / gammaSum) }

			val σs = SumArrays.sumColumnArrays(gammas.map{ case (v, gammaByCluster) => gammaByCluster.map{ case (clusterID, coef) => SumArrays.diffDotProduct(v, μs(clusterID)) *  coef / gammasSums(clusterID) } })

			val πksUpdated = gammasSums.map(_ / k)

			// Update parameters
			πksUpdated.zipWithIndex.foreach{ case (gammaz, clusterID) => πks(clusterID) = gammaz }
			gaussianLawFeatures.foreach{ case (clusterID, _) => gaussianLawFeatures(clusterID) = (clusterID, (μs(clusterID), σs(clusterID))) }

			val zipedμs = μs.zipWithIndex
			allModsHaveConverged = zipedμs.forall{ case (updatedMean, clusterID) => metric.d(updatedMean, checkingMeans(clusterID)) <= epsilon }
			zipedμs.foreach{ case (updatedMean, clusterID) => checkingMeans(clusterID) = updatedMean }
			
			cpt += 1
		}

		val finalAffectation = data.map( v =>
		{
			val gammaByCluster = Stats.obtainGammaByCluster(v, gaussianLawFeatures, πks)
			val clusterID = gammaByCluster.maxBy(_._2)._1
			(clusterID, v, gammaByCluster.map(_._2))
		})

		val groupedByClusterID = finalAffectation.groupBy(_._1).toSeq
		val finalCentroids = mutable.HashMap(groupedByClusterID.map{ case  (clusterID, aggregate) => (clusterID, SumArrays.sumColumnArrays(aggregate.map(_._2))) }:_*)
		val finalCardinalities = mutable.HashMap(groupedByClusterID.map{ case  (clusterID, aggregate) => (clusterID, aggregate.size) }:_*)

		new GaussianMixtureModel(finalCentroids, finalCardinalities, metric, finalAffectation)
	}
}

object GaussianMixture extends DataSetsTypes[Int, Array[Double]]
{
	/**
	 * Run the Gaussian Mixture
	 **/
	def run(data: Array[Vector], k: Int, epsilon: Double, iterMax: Int, metric: ContinuousDistances = new Euclidean(true)): GaussianMixtureModel =
	{
		val gaussianMixture = new GaussianMixture(data, k, epsilon, iterMax, metric)
		val gaussianMixtureModel = gaussianMixture.run()
		gaussianMixtureModel
	}
}