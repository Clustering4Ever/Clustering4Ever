package org.clustering4ever.clusteringanalysis
/**
 * @author Beck Gaël
 */
import scala.collection.{mutable, immutable}
import org.clustering4ever.clustering.ClusteringSharedTypes
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusterIDType._
/**
 *
 */
trait ClusteringBinaryAnalysisAncestor extends ClusteringSharedTypes {
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
	lazy val occurencesRatioPerFeatureByClusterID: immutable.HashMap[ClusterID, Seq[Double]] = {
		occurencesPerFeatureByClusterID.map{ case (clusterID, featsOcc) => (clusterID, featsOcc.zip(occurencesPerFeature).map{ case (a, b) => a.toDouble / b }) }
	}
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
) extends ClusteringBinaryAnalysisAncestor
/**
 *
 */
trait EveryClusteringBinaryAnalysisAncestor extends ClusteringSharedTypes {
	/**
	 * ClusteringBinaryAnalysis by clusteringNumber by vectorizationID
	 */
	val clusteringBinaryAnalysisByClusteringNumberByVectorization: mutable.HashMap[VectorizationID, mutable.HashMap[ClusteringNumber, ClusteringBinaryAnalysis]]
	/**
	 * @return an Option of the ClusteringBinaryAnalysis corresponding to given ClusteringNumber
	 */
	def getStatsByClusteringNumber(clusteringNumber: ClusteringNumber): Option[ClusteringBinaryAnalysis] = {
		clusteringBinaryAnalysisByClusteringNumberByVectorization.find(_._2.contains(clusteringNumber)).map(_._2(clusteringNumber))
	}
}
/**
 *
 */
case class EveryClusteringBinaryAnalysis(
	val clusteringBinaryAnalysisByClusteringNumberByVectorization: mutable.HashMap[VectorizationID, mutable.HashMap[ClusteringNumber, ClusteringBinaryAnalysis]] = mutable.HashMap.empty[VectorizationID, mutable.HashMap[ClusteringNumber, ClusteringBinaryAnalysis]]
) extends EveryClusteringBinaryAnalysisAncestor