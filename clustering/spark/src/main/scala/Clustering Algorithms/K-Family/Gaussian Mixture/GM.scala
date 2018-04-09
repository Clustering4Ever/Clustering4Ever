package clustering4ever.spark.clustering.gaussianmixtures

import _root_.clustering4ever.clustering.datasetstype.DataSetsTypes
import _root_.clustering4ever.clustering.ClusteringAlgorithms
import _root_.clustering4ever.math.distances.ContinuousDistances
import _root_.clustering4ever.util.SumArrays
import _root_.clustering4ever.stats.Stats
import _root_.clustering4ever.scala.kernels.Kernels
import _root_.scala.math.{min, max, sqrt, pow}
import _root_.scala.collection.{immutable, mutable}
import _root_.scala.util.Random
import _root_.org.apache.spark.rdd.RDD


/**
 * @author Beck Gaël
 * Gaussian Mixtures using a user-defined dissmilarity measure.
 * @param data : an Array with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider a centroid has converged
 * @param iterMax : maximal number of iteration
 * @param metric : a defined dissimilarity measure, it can be custom by overriding ContinuousDistances distance function
 **/
class GaussianMixtures(
	data: RDD[Array[Double]],
	var k: Int,
	var epsilon: Double,
	var iterMax: Int,
	var metric: ContinuousDistances
) extends ClusteringAlgorithms[Long, Array[Double]]
{
	val dim = data.first.size
	/**
	 * Simplest centroids initializations
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
			val minAndMax = for( i <- vectorRange ) yield( obtainMinMax(i, minMaxa, minMaxb) )
			minAndMax.unzip
		})

		val ranges = minv.zip(maxv).map{ case (min, max) => (max - min, min) }
		val centroids = mutable.HashMap((0 until k).map( clusterID => (clusterID, ranges.map{ case (range, min) => Random.nextDouble * range + min }) ):_*)
		centroids
	}

	/**
	 * Run the Gaussian Mixture
	 **/
	def run(): GaussianMixturesModel =
	{
		val centroids = initializationCentroids()
		val centroidsAsArray = centroids.toArray
		val clustersCardinality = centroids.map{ case (clusterID, _) => (clusterID, 0) }

		def obtainNearestModID(v: Array[Double]): ClusterID =
		{
			centroidsAsArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.sortBy(_._2).head._1
		}

		// Allocation to nearest centroid
		val clusterized = data.map( v => (obtainNearestModID(v), v) )
	
		val neutralElement = mutable.ArrayBuffer.empty[Vector]
		def addToBuffer(buff: mutable.ArrayBuffer[Vector], elem: Vector) = buff += elem
		def aggregateBuff(buff1: mutable.ArrayBuffer[Vector], buff2: mutable.ArrayBuffer[Vector]) = buff1 ++= buff2

		val normalLawFeatures = mutable.HashMap(
			clusterized.aggregateByKey(neutralElement)(addToBuffer, aggregateBuff).map{ case (clusterID, aggregates) =>
			{
				val vectors = aggregates.toArray
				val meanC = Stats.mean(vectors)
				val sdC = Stats.sd(vectors, meanC)
				(clusterID, (meanC, sdC))
			}}.collect
		:_*)

		def reduceColumnsMatrixRDD(rdd: RDD[Vector]) =
		{
			rdd.reduce{ case (gamma1, gamma2) =>
			{
				for( i <- gamma1.indices.toArray ) yield (gamma1(i) + gamma2(i))
			}}
		}

		val zeroMod = Array.fill(dim)(0D)
		var cpt = 0
		var allModsHaveConverged = false
		val πks = normalLawFeatures.map{ case (clusterID, _) => (clusterID, 1D / k) }
		while( cpt < iterMax && ! allModsHaveConverged )
		{
			val gammas = data.map( v =>
			{
				val genProb = normalLawFeatures.toArray.map{ case (clusterID, (meanC, sdC)) => (clusterID, Kernels.gaussianKernel(v, meanC, 1D / pow(sdC, 2), metric)) }

				val averaging = genProb.map{ case (clusterID, prob) => prob * πks(clusterID) }.sum 

				val gammaByCluster = genProb.map{ case (clusterID, prob) => (clusterID, (πks(clusterID) * prob) / averaging) }.sortBy(_._1)

				(v, gammaByCluster)
			})


			val gammasSums = reduceColumnsMatrixRDD(gammas.map(_._2.map(_._2)))

			val μs = gammas.map{ case (v, gammaByCluster) =>  gammaByCluster.map{ case (clusterID, coef) => v.map(_ * coef / gammasSums(clusterID)) }}.reduce(Stats.reduceMultipleMatriceColumns)

			val σs = reduceColumnsMatrixRDD(gammas.map{ case (v, gammaByCluster) => gammaByCluster.map{ case (clusterID, coef) => Stats.diffDotProduct(v, μs(clusterID)) *  coef / gammasSums(clusterID)  }})

			val πksUpdated = gammasSums.map(_ / k)


			// Update parameters
			πksUpdated.zipWithIndex.foreach{ case (gammaz, clusterID) => πks(clusterID) = gammaz }
			normalLawFeatures.foreach{ case (clusterID, _) => normalLawFeatures(clusterID) = (μs(clusterID), σs(clusterID)) }



			val kModesBeforeUpdate = centroids.clone

			// Reinitialization of centroids
			centroids.foreach{ case (clusterID, mod) => centroids(clusterID) = zeroMod }
			clustersCardinality.foreach{ case (clusterID, _) => clustersCardinality(clusterID) = 0 }

			// Updatating means
			clusterized.foreach{ case (clusterID, v) =>
			{
				centroids(clusterID) = SumArrays.sumArraysNumerics(centroids(clusterID), v)
				clustersCardinality(clusterID) += 1
			}}

			centroids.foreach{ case (clusterID, mod) => centroids(clusterID) = mod.map(_ / clustersCardinality(clusterID)) }

			allModsHaveConverged = kModesBeforeUpdate.forall{ case (clusterID, previousMod) => metric.d(previousMod, centroids(clusterID)) <= epsilon }

			cpt += 1
		}
		new GaussianMixturesModel(centroids, clustersCardinality, metric)
	}
}

object GaussianMixtures extends DataSetsTypes[Long, Array[Double]]
{
	/**
	 * Run the Gaussian Mixture
	 **/
	def run(data: RDD[Vector], k: Int, epsilon: Double, iterMax: Int, metric: ContinuousDistances): GaussianMixturesModel =
	{
		val kMeans = new GaussianMixtures(data, k, epsilon, iterMax, metric)
		val kmeansModel = kMeans.run()
		kmeansModel
	}
}