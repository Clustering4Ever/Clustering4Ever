package org.clustering4ever.scala.clustering.gaussianmixture
/**
 * @author Beck Gaël
 */

import scala.collection.mutable
import org.clustering4ever.math.distances.ContinuousDistance
import org.clustering4ever.clustering.ClusteringModel
import scala.collection.GenSeq
import breeze.linalg.{DenseVector, DenseMatrix}
import scala.math.{sqrt, pow, Pi}

final case class GaussianMixtureModel(val gaussianLawFeaturesSortedByClusterID: Array[GaussianLawFeatures], val πksortedByClusterID: mutable.Map[Int, Double]) extends ClusteringModel {

	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.GaussianMixtures

	def predict(v: DenseVector[Double]): Int =
		GammaComputation.obtainGammaByCluster(v, gaussianLawFeaturesSortedByClusterID, πksortedByClusterID)._1.maxBy(_._2)._1

	def predict(gs: GenSeq[DenseVector[Double]]): GenSeq[Int] = gs.map(predict)

}
