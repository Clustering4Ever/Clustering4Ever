package org.clustering4ever.clustering.kfamily.gaussianmixture

/**
 * @author Beck Gaël
 */

import breeze.linalg.DenseVector
import org.clustering4ever.clusteringtraits.ClusteringModel
import org.clustering4ever.roottraits.GaussianMixtures

import scala.collection.{GenSeq, mutable}

final case class GaussianMixtureModel(val gaussianLawFeaturesSortedByClusterID: Array[GaussianLawFeatures], val πksortedByClusterID: mutable.Map[Int, Double]) extends ClusteringModel {

	val algorithmID = GaussianMixtures

	def predict(v: DenseVector[Double]): Int =
		GammaComputation.obtainGammaByCluster(v, gaussianLawFeaturesSortedByClusterID, πksortedByClusterID)._1.maxBy(_._2)._1

	def predict(gs: GenSeq[DenseVector[Double]]): GenSeq[Int] = gs.map(predict)

}
