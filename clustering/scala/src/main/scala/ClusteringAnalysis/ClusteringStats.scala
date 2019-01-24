package org.clustering4ever.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.collection.immutable
import org.clustering4ever.clustering.ClusteringCommons
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.types.ClusteringNumberType._
/**
 *
 */
case class ClusteringBinaryAnalysis(
	val occurencesPerFeature: Seq[Int],
	val frequencyPerFeature: Seq[Double],
	val occurencesPerFeatureByClusterID: immutable.HashMap[Int, Seq[Int]],
	val frequencyPerFeatureByClusterID: immutable.HashMap[Int, Seq[Double]]
) extends ClusteringCommons {
	/**
	 * HashMap of each feature occurence ratio by clusterID, compared to global features 
	 */
	val occurencesRatioPerFeatureByClusterID = occurencesPerFeatureByClusterID.map{ case (clusterID, featsOcc) => (clusterID, featsOcc.zip(occurencesPerFeature).map{ case (a, b) => a.toDouble / b }) }
}
/**
 *
 */
case class EveryClusteringBinaryAnalysis(
	val byVectorizationByCN: immutable.HashMap[VectorizationID, immutable.HashMap[ClusteringNumber, ClusteringBinaryAnalysis]] = immutable.HashMap.empty[VectorizationID, immutable.HashMap[ClusteringNumber, ClusteringBinaryAnalysis]]
) extends ClusteringCommons {
	/**
	 *
	 */
	def getStatsByClusteringNumber(clusteringNumber: ClusteringNumber): Option[ClusteringBinaryAnalysis] = {
		byVectorizationByCN.find(_._2.contains(clusteringNumber)).map(_._2(clusteringNumber))
	}
}