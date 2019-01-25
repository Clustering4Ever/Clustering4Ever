package org.clustering4ever.clusteringanalysis
/**
 * @author Beck Gaël
 */
import scala.collection.immutable
import org.clustering4ever.clustering.ClusteringCommons
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusterIDType._
/**
 *
 */
trait ClusteringBinaryAnalysisAncestor extends ClusteringCommons {
	/**
	 * The number of clustering which was applied 
	 */
	val clusteringNumber: ClusteringNumber
	/**
	 * Number of occurences for each features
	 */
	val occurencesPerFeature: Seq[Int]
	/**
	 * Frequencies for each features
	 */
	val frequencyPerFeature: Seq[Double]
	/**
	 * Number of occurences for each features by ClusterID
	 */
	val occurencesPerFeatureByClusterID: immutable.HashMap[ClusterID, Seq[Int]]
	/**
	 * Frequencies for each features by ClusterID
	 */
	val frequencyPerFeatureByClusterID: immutable.HashMap[ClusterID, Seq[Double]]	
	/**
	 * HashMap of each feature occurence ratio by clusterID, compared to global features 
	 */
	val occurencesRatioPerFeatureByClusterID: immutable.HashMap[ClusterID, Seq[Double]]
}
/**
 * This class gather various informations about a specific clustering on binary data 
 */
case class ClusteringBinaryAnalysis(
	val clusteringNumber: ClusteringNumber,
	val occurencesPerFeature: Seq[Int],
	val frequencyPerFeature: Seq[Double],
	val occurencesPerFeatureByClusterID: immutable.HashMap[ClusterID, Seq[Int]],
	val frequencyPerFeatureByClusterID: immutable.HashMap[ClusterID, Seq[Double]]
) extends ClusteringBinaryAnalysisAncestor {

	lazy val occurencesRatioPerFeatureByClusterID = occurencesPerFeatureByClusterID.map{ case (clusterID, featsOcc) => (clusterID, featsOcc.zip(occurencesPerFeature).map{ case (a, b) => a.toDouble / b }) }
}
/**
 *
 */
trait EveryClusteringBinaryAnalysisAncestor extends ClusteringCommons {
	/**
	 * ClusteringBinaryAnalysis by clusteringNumber by vectorizationID
	 */
	val byVectorizationByClusteringNumber: immutable.HashMap[VectorizationID, immutable.HashMap[ClusteringNumber, ClusteringBinaryAnalysis]]
	/**
	 * @return an Option of the ClusteringBinaryAnalysis corresponding to given ClusteringNumber
	 */
	def getStatsByClusteringNumber(clusteringNumber: ClusteringNumber): Option[ClusteringBinaryAnalysis] = {
		byVectorizationByClusteringNumber.find(_._2.contains(clusteringNumber)).map(_._2(clusteringNumber))
	}
}
/**
 *
 */
case class EveryClusteringBinaryAnalysis(
	val byVectorizationByClusteringNumber: immutable.HashMap[VectorizationID, immutable.HashMap[ClusteringNumber, ClusteringBinaryAnalysis]] = immutable.HashMap.empty[VectorizationID, immutable.HashMap[ClusteringNumber, ClusteringBinaryAnalysis]]
) extends EveryClusteringBinaryAnalysisAncestor