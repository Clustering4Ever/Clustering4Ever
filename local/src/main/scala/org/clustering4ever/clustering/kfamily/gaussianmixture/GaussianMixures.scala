package org.clustering4ever.clustering.kfamily.gaussianmixture

/**
 * @author Beck Gaël
 */
import breeze.linalg.{DenseMatrix, DenseVector, det, inv, pinv}
import org.clustering4ever.clusteringtraits.ClusteringAlgorithm
import org.clustering4ever.distances.continuous.Euclidean
import org.clustering4ever.kernels.GmmKernels
import org.clustering4ever.math.Stats
import org.clustering4ever.roottraits.{Clusterizable, GVector, GaussianMixtures, ScalarVector}
import org.clustering4ever.util.SumVectors

import scala.collection.{GenSeq, mutable}
import scala.language.higherKinds
import scala.math.{Pi, pow, sqrt}
import scala.util.{Random, Try}

/**
 *
 */
object GammaComputation {

	/**
	 *
	 */
	def obtainPreLikelyhood(v: DenseVector[Double], gaussianLawFeaturesSortedByClusterID: Array[GaussianLawFeatures], πkSortedByClusterID: mutable.Map[Int, Double]): (Array[(Int, Double)], Double) = {
		val weights = gaussianLawFeaturesSortedByClusterID.map{ case GaussianLawFeatures(clusterID, mean, det, invSigma) =>
			val kernelVal = GmmKernels.multivariateGaussianKernel(v, mean, invSigma)
			val dim = mean.size
			val cst = sqrt(Math.pow(2 * Pi, dim) * Math.abs(det))
			val normalLaw = kernelVal / cst
			(clusterID,	normalLaw)
		}

		val preLikelyhood = weights.foldLeft(0D){ case (agg, (clusterID, nl)) => agg + nl * πkSortedByClusterID(clusterID) }
		(weights, preLikelyhood)
	}

	/**
	 *
	 */
	def obtainGammaByCluster(v: DenseVector[Double], gaussianLawFeaturesSortedByClusterID: Array[GaussianLawFeatures], πkSortedByClusterID: mutable.Map[Int, Double]): (Array[(Int, Double)], Double) = {

		val (weights, preLikelyhood) = obtainPreLikelyhood(v, gaussianLawFeaturesSortedByClusterID, πkSortedByClusterID)

		val gammaByCluster = weights.map{ case (clusterID, kernelVal) =>
			val gamma = (kernelVal * πkSortedByClusterID(clusterID)) / preLikelyhood
			(clusterID, gamma)
		}

		(gammaByCluster, preLikelyhood)

	}

}
/**
 *
 */
final case class GaussianLawFeatures(clusterID: Int, mean: DenseVector[Double], detSigma: Double, invSigma: DenseMatrix[Double])

/**
 * Gaussian Mixtures
 * @param data : an Seq with and ID and the vector
 * @param k : number of clusters
 * @param epsilon : minimal threshold under which we consider log-likelihood does not moove enough
 * @param iterMax : maximal number of iteration
 */
class GaussianMixture[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz]](
	k: Int,
	epsilon: Double,
	iterMax: Int
) extends ClusteringAlgorithm {

	val algorithmID = GaussianMixtures

	val metric = new Euclidean(true)

	/**
	 * Run the Gaussian Mixture
	 */
	def run(data: GenSeq[Cz[O, ScalarVector]]) = {

		def kmppInitialization(k: Int): mutable.HashMap[Int, DenseVector[Double]] = {

			def obtainNearestCenter(v: ScalarVector, centers: mutable.ArrayBuffer[ScalarVector]): ScalarVector =
				centers.minBy(metric.d(_, v))

			val centersBuff = mutable.ArrayBuffer(data(Random.nextInt(data.size)).v)
			
			(1 until k).foreach{ i =>
				centersBuff += Stats.obtainMedianFollowingWeightedDistribution[ScalarVector](data.map{ cz =>
					val toPow2 = metric.d(cz.v, obtainNearestCenter(cz.v, centersBuff))
					(cz.v, toPow2 * toPow2)
				}.toBuffer)
			}

			val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, new DenseVector(center.vector)) }:_*)
			centers
		}

		val vectorizedDataset: GenSeq[DenseVector[Double]] = data.map( cz => new DenseVector(cz.v.vector))
		val centers: mutable.HashMap[Int, DenseVector[Double]] = kmppInitialization(k)
		val dim = vectorizedDataset.head.size
		val gammaCst = pow(2 * Pi, dim / 2D)
		val clusterIDs = (0 until k)

		val centersInit = centers.map{ case (_, center) => ScalarVector(center.toArray) }.toArray.zipWithIndex

		def obtainNearestCenterInit(v: ScalarVector, centers: Array[(ScalarVector, Int)]): Int =
			centers.minBy{ case (c, _) => metric.d(c, v) }._2


		def obtainPreSigma(dot: DenseVector[Double], mean: DenseVector[Double]): DenseMatrix[Double] = {
			val diff = dot - mean
			diff * diff.t
		}

		val safeDetCst = 1E-10

		val gaussianLawFeatures: Array[GaussianLawFeatures] = data.map{ cz => 
			(obtainNearestCenterInit(cz.v, centersInit), cz.v.vector)
		}.groupBy(_._1).seq
		.map{ case (clusterID, cluster) =>


			val mean = centersInit(clusterID)._1.vector
			val dvmean = new DenseVector(centersInit(clusterID)._1.vector)

			val covMatrix = cluster.map{ case (_, v) => obtainPreSigma(new DenseVector(v), dvmean) }
				.reduce(_ + _)
				.map(_ / (cluster.size - 1))


			val covMatrixDet = det(covMatrix)
			val safeCovMatrixDet = if (covMatrixDet == 0D) safeDetCst else covMatrixDet
			val invMatrix = Try(inv(covMatrix)).getOrElse(pinv(covMatrix))

			GaussianLawFeatures(clusterID, dvmean, safeCovMatrixDet, invMatrix)

		}.toArray

		val checkingMeans: Array[Array[Double]] = gaussianLawFeatures.map{ case GaussianLawFeatures(_, mean, _, _) => mean.toArray }

		var cpt = 0
		var logLikelihoodRelativeDiff = false
		val πks = mutable.Map(gaussianLawFeatures.map{ case GaussianLawFeatures(clusterID, _, _, _) => (clusterID, 1D / k) }:_*)

		import org.clustering4ever.util.VectorsAddOperationsImplicits._

		def maximization(gammas: GenSeq[(DenseVector[Double], (Array[(Int, Double)], Double))], gammasSums: mutable.Map[Int, Double]): (mutable.Buffer[Array[Double]], Array[DenseMatrix[Double]], mutable.Map[Int, Double]) = {

			val πksUpdated = mutable.Map(gammasSums.mapValues(_ / data.size).toSeq:_*)

			val μs: mutable.Buffer[Array[Double]] = gammas.map{ case (v, (gammaByCluster, _)) =>
				gammaByCluster.map{ case (_, gamma) =>
					(v * gamma).toArray
				}.toBuffer
			}
			.reduce(_.zip(_).map{ case (a, b) => SumVectors.sumVectors(a, b) })
			.zipWithIndex
			.map{ case (v, clusterID) => v.map(_ / gammasSums(clusterID)) }

			val σs: Array[DenseMatrix[Double]] = gammas.map{ case (v, (gammaByCluster, _)) =>
					gammaByCluster.map{ case (clusterID, gamma) =>
						obtainPreSigma(v, DenseVector(μs(clusterID))) * gamma
					}
				}
				.reduce(_.zip(_).map{ case (m1, m2) => m1 + m2 })
				.zipWithIndex
				.map{ case (preσ, clusterID) => preσ / gammasSums(clusterID) }

			(μs, σs, πksUpdated)
		}

		var previousLogLikelyhood = Double.PositiveInfinity

		while (cpt < iterMax && !logLikelihoodRelativeDiff) {

			// Expectation step
			val gammas: GenSeq[(DenseVector[Double], (Array[(Int, Double)], Double))] = vectorizedDataset.map{ v =>
				(v, GammaComputation.obtainGammaByCluster(v, gaussianLawFeatures, πks))
			}

			val logLikelyhood = gammas.aggregate(0D)({ case (agg, (_, (_, e))) => agg + Math.log(e) }, _ + _)

			val gammasSumSeq = gammas.map{ case (_, (gammaByCluster, _)) =>
				gammaByCluster.map{ case (_, gamma) => gamma }
			}
			.reduce(_.zip(_).map{ case (a, b) => a + b })
			.zipWithIndex
			.map(_.swap)

			val gammasSums: mutable.Map[Int, Double] = mutable.Map(gammasSumSeq:_*)

			val (μs, σs, πksUpdated) = maximization(gammas, gammasSums)
			
			// Update parameters
			clusterIDs.foreach{ clusterID =>

				πks(clusterID) = πksUpdated(clusterID)

				val mean = DenseVector(μs(clusterID))
				val sigma = σs(clusterID)
				val detSigma = det(sigma)
				val safeDetSigma = if (detSigma == 0D) safeDetCst else detSigma
				val invSigma = Try(inv(sigma)).getOrElse(pinv(sigma))
				
				gaussianLawFeatures(clusterID) = GaussianLawFeatures(clusterID, mean, safeDetSigma, invSigma)

			}

			// Stopping criteria
			logLikelihoodRelativeDiff = Math.abs((previousLogLikelyhood - logLikelyhood) / logLikelyhood) <= epsilon
			previousLogLikelyhood = logLikelyhood

			μs.zipWithIndex.foreach{ case (updatedMean, clusterID) =>
				checkingMeans(clusterID) = updatedMean
			}

			cpt += 1
		}

		GaussianMixtureModel(gaussianLawFeatures, πks)
	}
}

object GaussianMixture {

	/**
	 * Run the Gaussian Mixture
	 **/
	def run[O, Cz[O, V <: GVector[V]] <: Clusterizable[O, V, Cz]](
		data: GenSeq[Cz[O, ScalarVector]],
		k: Int,
		epsilon: Double = 0.0001,
		iterMax: Int = 100
	) = {
		val gaussianMixture = new GaussianMixture[O, Cz](k, epsilon, iterMax)
		val gaussianMixtureModel = gaussianMixture.run(data)
		gaussianMixtureModel
	}

}