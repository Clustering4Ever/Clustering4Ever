package org.clustering4ever.qualitymeasures

/**
 * @author Beck Gaël
 */
import org.clustering4ever.distances.Distance
import org.clustering4ever.roottraits.ExternalsIndices.{ExternalsIndicesType, _}
import org.clustering4ever.roottraits.InternalsIndices.{InternalsIndicesType, _}
import org.clustering4ever.roottraits.{Clusterizable, ClustersIndicesAnalysis, GVector}
import org.clustering4ever.roottraits.ClusteringNumberType._

import scala.collection.{GenSeq, immutable}
import scala.language.higherKinds
/**
 *
 */
trait ClustersIndicesAnalysisAncestorLocal[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    GS[X] <: GenSeq[X]
] extends ClustersIndicesAnalysis[O, V, Cz, GS] {

    final def obtainInternalsIndices[D <: Distance[V]](metric: D, clusteringNumber: ClusteringNumber, indices: InternalsIndicesType*): immutable.Map[InternalsIndicesType, Double] = {
        val tmpMetric = metric
        val internalIndices = new InternalIndicesAncestorLocal[V, D] { val metric = tmpMetric }
        indices.par.map{ index =>
            index match {
                case DaviesBouldin => (DaviesBouldin, internalIndices.daviesBouldin(clusterized, clusteringNumber))
                case BallHall => (BallHall, internalIndices.ballHall(clusterized, clusteringNumber))
                // case Silhouette => (Silhouette, internalIndices.silhouette(clusterized, clusteringNumber))
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
    }
    /**
     *
     */
    final def computeInternalsIndicesOfEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): Seq[immutable.Map[InternalsIndicesType, Double]] = {
        clusterized.head.clusterIDs.indices.par.map(cn => obtainInternalsIndices(metric, cn, indices:_*) ).seq
    }
    /**
     * Compute given externals indices and add result to externalsIndicesByClusteringNumber
     * @return A Map which link external indices to its associate value 
     */
    final def computeExternalsIndices(groundTruth: GS[ClusterID], clusteringNumber: ClusteringNumber, indices: ExternalsIndicesType*): immutable.Map[ExternalsIndicesType, Double] = {

        val onlyClusterIDs = clusterized.map(_.clusterIDs(clusteringNumber))
        val targetAndPred = groundTruth.zip(onlyClusterIDs)

        val authorizedValues = immutable.Set(0, 1)
        val isBinary = !targetAndPred.exists{ case (target, pred) => !authorizedValues.contains(target) || !authorizedValues.contains(pred) }

        val obtainedIndices = {

            if(isBinary) {
                val externalIndices = BinaryExternalIndicesLocal(targetAndPred)
                indices.par.map{ index =>
                    index match {
                        case MI => (MI, externalIndices.mutualInformation)
                        case NMI_Sqrt => (NMI_Sqrt, externalIndices.nmiSQRT)
                        case NMI_Max => (NMI_Max, externalIndices.nmiMAX)
                        case Purity => (Purity, externalIndices.purity)
                        case Accuracy => (Accuracy, externalIndices.accuracy) 
                        case Precision => (Precision, externalIndices.precision) 
                        case Recall => (Recall, externalIndices.recall) 
                        case F1 => (F1, externalIndices.f1) 
                        case _ => throw new IllegalArgumentException("Asked index is not repertoried")
                    }
                }.seq.toMap
            }
            else {
                val externalIndices = MultiExternalIndicesLocal(targetAndPred)
                indices.par.map{ index =>
                    index match {
                        case MI => (MI, externalIndices.mutualInformation)
                        case NMI_Sqrt => (NMI_Sqrt, externalIndices.nmiSQRT)
                        case NMI_Max => (NMI_Max, externalIndices.nmiMAX)
                        case Purity => (Purity, externalIndices.purity)
                        // case MCC => (MCC, externalIndices.mcc) 
                        // case CzekanowskiDice => (CzekanowskiDice, externalIndices.czekanowskiDice) 
                        // case RAND => (RAND, externalIndices.rand) 
                        // case RogersTanimoto => (RogersTanimoto, externalIndices.rogersTanimoto) 
                        // case FolkesMallows => (FolkesMallows, externalIndices.folkesMallows) 
                        // case Jaccard => (Jaccard, externalIndices.jaccard) 
                        // case Kulcztnski => (Kulcztnski, externalIndices.kulcztnski) 
                        // case McNemar => (McNemar, externalIndices.mcNemar) 
                        // case RusselRao => (RusselRao, externalIndices.russelRao) 
                        // case SokalSneath1 => (SokalSneath1, externalIndices.sokalSneath1) 
                        // case SokalSneath2 => (SokalSneath2, externalIndices.sokalSneath2)
                        case _ => throw new IllegalArgumentException("Asked index is not repertoried or you asked for a binary index with more than 2 class")
                    }
                }.seq.toMap                
            }
        }
        externalsIndicesByClusteringNumber += ((clusteringNumber, obtainedIndices))
        obtainedIndices
    }
    /**
     *
     */
    final def computeExternalsIndicesOfEveryClusteringNumber(groundTruth: GS[ClusterID], indices: ExternalsIndicesType*): Seq[immutable.Map[ExternalsIndicesType, Double]] = {
        clusterized.head.clusterIDs.indices.par.map(cn => computeExternalsIndices(groundTruth, cn, indices:_*) ).seq
    }   
}
/**
 *
 */
final case class ClustersIndicesAnalysisLocal[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    GS[X] <: GenSeq[X]
](final val clusterized: GS[Cz[O, V]]) extends ClustersIndicesAnalysisAncestorLocal[O, V, Cz, GS]