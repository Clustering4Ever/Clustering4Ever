package org.clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{GenSeq, immutable, mutable}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.ClustersIndicesAnalysis
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.scala.indices.{ExternalIndices, InternalIndices}
import org.clustering4ever.enums.NmiNormalizationNature
import org.clustering4ever.enums.InternalsIndices.InternalsIndicesType
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices.ExternalsIndicesType
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.vectors.GVector
import org.clustering4ever.shapeless.DistancesMapping
import shapeless.HMap
/**
 *
 */
class ClustersIndicesAnalysisLocal[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
](val data: GS[Cz[ID, O, V]], override val internalsIndicesByClusteringNumber: mutable.HashMap[(Int, Int, InternalsIndicesType), Double] = mutable.HashMap.empty[(Int, Int, InternalsIndicesType), Double]) extends ClustersIndicesAnalysis[ID, O, V, Cz, GS] {
    /**
     *
     */
    type Self = ClustersIndicesAnalysisLocal[ID, O, V, Cz, GS]
    /**
     * Compute given internals indices and add result to internalsIndicesByClusteringNumber
     * @return A Map which link internal indices to its associate value 
     */
    def obtainInternalsIndices[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: Int = 0): Map[InternalsIndicesType, Double] = {
        
        val idAndVector: GS[(ClusterID, V)] = data.map( cz => (cz.clusterIDs(clusteringNumber), cz.v) ).asInstanceOf[GS[(ClusterID, V)]]

        val internalIndices = new InternalIndices(idAndVector, metric)

        indices.par.map{ index =>
            index match {
                case DaviesBouldin => (DaviesBouldin, internalIndices.daviesBouldin)
                case BallHall => (BallHall, internalIndices.ballHall)
                case Silhouette => (Silhouette, internalIndices.silhouette)
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
    }
    /**
     * Compute given internals indices and add result to internalsIndicesByClusteringNumber
     * @return A Map which link internal indices to its associate value 
     */
    def saveInternalsIndices[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: Int = 0): ClustersIndicesAnalysisLocal[ID, O, V, Cz, GS] = {
        
        val idAndVector: GS[(ClusterID, V)] = data.map( cz => (cz.clusterIDs(clusteringNumber), cz.v) ).asInstanceOf[GS[(ClusterID, V)]]

        val obtainedIndices = obtainInternalsIndices(metric, indices:_*)(clusteringNumber).seq.map{ case (indexType, v) => ((metric.id, clusteringNumber, indexType), v) }
        
        new ClustersIndicesAnalysisLocal(data, internalsIndicesByClusteringNumber ++= obtainedIndices)
    }
    /**
     *
     */
    def computeInternalsIndicesForEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): Seq[Map[InternalsIndicesType, Double]] = {
        (0 until data.head.clusterIDs.size).par.map( cn => obtainInternalsIndices(metric, indices:_*)(cn) ).seq
    }
    /**
     *
     */
    def saveInternalsIndicesForEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): ClustersIndicesAnalysisLocal[ID, O, V, Cz, GS] = {
        
        val indicess = computeInternalsIndicesForEveryClusteringNumber(metric, indices:_*).zipWithIndex.flatMap{ case (scores, idx) => scores.map{ case (indexType, v) => ((metric.id, idx, indexType), v) } }

        new ClustersIndicesAnalysisLocal(data, internalsIndicesByClusteringNumber ++= indicess)
    }
    /**
     * Compute given externals indices and add result to externalsIndicesByClusteringNumber
     * @return A Map which link external indices to its associate value 
     */
    def computeExternalsIndices(groundTruth: GS[ClusterID], indices: ExternalsIndicesType*)(clusteringNumber: Int = 0): Map[ExternalsIndicesType, Double] = {

        val onlyClusterIDs = data.map(_.clusterIDs(clusteringNumber))

        val obtainedIndices = indices.par.map{ index =>
            index match {
                case MI => (MI, ExternalIndices.mutualInformation(onlyClusterIDs, groundTruth))
                case NMI_Sqrt => (NMI_Sqrt, ExternalIndices.nmi(onlyClusterIDs, groundTruth, NmiNormalizationNature.SQRT)) 
                case NMI_Max => (NMI_Max, ExternalIndices.nmi(onlyClusterIDs, groundTruth, NmiNormalizationNature.MAX))
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
        
        externalsIndicesByClusteringNumber += ((clusteringNumber, obtainedIndices))
        
        obtainedIndices
    }
    /**
     *
     */
    def computeExternalsIndicesForEveryClusteringNumber(groundTruth: GS[ClusterID], indices: ExternalsIndicesType*): Seq[Map[ExternalsIndicesType, Double]] = {
        (0 until data.head.clusterIDs.size).par.map( cn => computeExternalsIndices(groundTruth, indices:_*)(cn) ).seq
    }
}