package org.clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.GenSeq
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.ClustersIndexesAnalysis
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.scala.indexes.{ExternalIndexes, InternalIndexes}
import org.clustering4ever.enums.NmiNormalizationNature
import org.clustering4ever.enums.InternalsIndexes.InternalsIndexesType
import org.clustering4ever.enums.InternalsIndexes._
import org.clustering4ever.enums.ExternalsIndexes.ExternalsIndexesType
import org.clustering4ever.enums.ExternalsIndexes._
import org.clustering4ever.scala.vectors.GVector
/**
 *
 */
class ClustersIndexesAnalysisLocal[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
](val data: GS[Cz[ID, O, V]]) extends ClustersIndexesAnalysis[ID, O, V, Cz, GS] {
    /**
     * Compute given internals indexes and add result to internalsIndexesByClusteringNumber
     * @return A Map which link internal indexes to its associate value 
     */
    def computeInternalsIndexes[D <: Distance[V]](metric: D, indexes: InternalsIndexesType*)(clusteringNumber: Int = 0): Map[InternalsIndexesType, Double] = {
        
        val idAndVector: GS[(ClusterID, V)] = data.map( cz => (cz.clusterIDs(clusteringNumber), cz.v) ).asInstanceOf[GS[(ClusterID, V)]]

        val internalIndexes = new InternalIndexes(idAndVector, metric)

        val obtainedIndexes = indexes.par.map{ index =>
            index match {
                case DaviesBouldin => (DaviesBouldin, internalIndexes.daviesBouldin)
                case BallHall => (BallHall, internalIndexes.ballHall)
                case Silhouette => (Silhouette, internalIndexes.silhouette)
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
        
        internalsIndexesByClusteringNumber += ((clusteringNumber, obtainedIndexes))
        
        obtainedIndexes
    }
    /**
     *
     */
    def computeInternalsIndexesForEveryClusteringNumber[D <: Distance[V]](metric: D, indexes: InternalsIndexesType*): Seq[Map[InternalsIndexesType, Double]] = {
        (0 until data.head.clusterIDs.size).par.map( cn => computeInternalsIndexes(metric, indexes:_*)(cn) ).seq
    }
    /**
     * Compute given externals indexes and add result to externalsIndexesByClusteringNumber
     * @return A Map which link external indexes to its associate value 
     */
    def computeExternalsIndexes(groundTruth: GS[ClusterID], indexes: ExternalsIndexesType*)(clusteringNumber: Int = 0): Map[ExternalsIndexesType, Double] = {

        val onlyClusterIDs = data.map(_.clusterIDs(clusteringNumber))

        val obtainedIndexes = indexes.par.map{ index =>
            index match {
                case MI => (MI, ExternalIndexes.mutualInformation(onlyClusterIDs, groundTruth))
                case NMI_Sqrt => (NMI_Sqrt, ExternalIndexes.nmi(onlyClusterIDs, groundTruth, NmiNormalizationNature.SQRT)) 
                case NMI_Max => (NMI_Max, ExternalIndexes.nmi(onlyClusterIDs, groundTruth, NmiNormalizationNature.MAX))
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
        
        externalsIndexesByClusteringNumber += ((clusteringNumber, obtainedIndexes))
        
        obtainedIndexes
    }
    /**
     *
     */
    def computeExternalsIndexesForEveryClusteringNumber[D <: Distance[V]](groundTruth: GS[ClusterID], indexes: ExternalsIndexesType*): Seq[Map[ExternalsIndexesType, Double]] = {
        (0 until data.head.clusterIDs.size).par.map( cn => computeExternalsIndexes(groundTruth, indexes:_*)(cn) ).seq
    }
}