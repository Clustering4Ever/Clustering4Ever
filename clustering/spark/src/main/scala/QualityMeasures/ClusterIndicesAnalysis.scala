package org.clustering4ever.clustering.indices
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{immutable, mutable}
import shapeless.HMap
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.enums.NmiNormalizationNature
import org.clustering4ever.enums.InternalsIndices.InternalsIndicesType
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices.ExternalsIndicesType
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.vectors.GVector
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.MetricIDType._
/**
 *
 */
final case class ClustersIndicesAnalysisDistributed[O, V <: GVector[V] : ClassTag, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](final val clusterized: RDD[Cz[O, V]], final val sc: SparkContext, persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY)(implicit ct: ClassTag[V]) extends ClustersIndicesAnalysis[O, V, Cz, RDD] {

    // final val internalsIndicesByMetricClusteringNumberIndex = immutable.HashMap.empty[(MetricID, ClusteringNumber, InternalsIndicesType), Double]
    /**
     *
     */
    final type Self = ClustersIndicesAnalysisDistributed[O, V, Cz]
    /**
     * Compute given internals indices and add result to internalsIndicesByMetricClusteringNumberIndex
     * @return A Map which link internal indices to its associate value 
     */
    final def obtainInternalsIndices[D <: Distance[V]](metric: D, clusteringNumber: ClusteringNumber, indices: InternalsIndicesType*): Map[InternalsIndicesType, Double] = {
        val tmpMetric = metric
        val tmpCt = ct
        val internalIndices = new InternalIndicesAncestorDistributed[V, D] {
            val metric = tmpMetric
            implicit val ct: ClassTag[V] = tmpCt
        }

        indices.map{ index =>
            index match {
                case DaviesBouldin => (DaviesBouldin, internalIndices.daviesBouldin(sc, clusterized, clusteringNumber))
                case BallHall => (BallHall, internalIndices.ballHall(clusterized, clusteringNumber))
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
    }
    /**
     *
     */
    final def computeInternalsIndicesOfEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): Seq[Map[InternalsIndicesType, Double]] = {
        (0 until clusterized.first.clusterIDs.size).par.map( cn => obtainInternalsIndices(metric, cn, indices:_*) ).seq
    }
    /**
     * Compute given externals indices and add result to externalsIndicesByClusteringNumber
     * @return A Map which link external indices to its associate value 
     */
    final def computeExternalsIndices(groundTruth: RDD[ClusterID], clusteringNumber: ClusteringNumber, indices: ExternalsIndicesType*): Map[ExternalsIndicesType, Double] = {

        val targetAndPred = groundTruth.zip(clusterized.map(_.clusterIDs(clusteringNumber))).persist(persistanceLVL)

        val externalIndicesDistributed = BinaryExternalIndicesDistributed(targetAndPred, persistanceLVL)

        val obtainedIndices = indices.par.map{ index =>
            index match {
                case MI => (MI, externalIndicesDistributed.mutualInformation)
                case NMI_Sqrt => (NMI_Sqrt, externalIndicesDistributed.nmiSQRT) 
                case NMI_Max => (NMI_Max, externalIndicesDistributed.nmiMAX)
                case Purity => (Purity, externalIndicesDistributed.purity)
                // case Accuracy => (Accuracy, externalIndicesDistributed.accuracy) 
                // case Precision => (Precision, externalIndicesDistributed.precision) 
                // case Recall => (Recall, externalIndicesDistributed.recall) 
                // case F1 => (F1, externalIndicesDistributed.f1) 
                // case MCC => (MCC, externalIndicesDistributed.mcc) 
                // case CzekanowskiDice => (CzekanowskiDice, externalIndicesDistributed.czekanowskiDice) 
                // case RAND => (RAND, externalIndicesDistributed.rand) 
                // case RogersTanimoto => (RogersTanimoto, externalIndicesDistributed.rogersTanimoto) 
                // case FolkesMallows => (FolkesMallows, externalIndicesDistributed.folkesMallows) 
                // case Jaccard => (Jaccard, externalIndicesDistributed.jaccard) 
                // case Kulcztnski => (Kulcztnski, externalIndicesDistributed.kulcztnski) 
                // case McNemar => (McNemar, externalIndicesDistributed.mcNemar) 
                // case RusselRao => (RusselRao, externalIndicesDistributed.russelRao) 
                // case SokalSneath1 => (SokalSneath1, externalIndicesDistributed.sokalSneath1) 
                // case SokalSneath2 => (SokalSneath2, externalIndicesDistributed.sokalSneath2)
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
        
        externalsIndicesByClusteringNumber += ((clusteringNumber, obtainedIndices))
        
        obtainedIndices
    }
    /**
     *
     */
    final def computeExternalsIndicesOfEveryClusteringNumber(groundTruth: RDD[ClusterID], indices: ExternalsIndicesType*): Seq[Map[ExternalsIndicesType, Double]] = {
        (0 until clusterized.first.clusterIDs.size).par.map( cn => computeExternalsIndices(groundTruth, cn, indices:_*) ).seq
    }
}