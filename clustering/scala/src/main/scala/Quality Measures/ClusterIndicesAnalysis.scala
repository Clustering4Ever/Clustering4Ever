package org.clustering4ever.clustering.indices
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{GenSeq, immutable, mutable}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.indices.{ExternalIndicesLocal, InternalIndicesLocal}
import org.clustering4ever.enums.NmiNormalizationNature
import org.clustering4ever.enums.InternalsIndices.InternalsIndicesType
import org.clustering4ever.clustering.ClusteringInformationsLocal
import org.clustering4ever.vectorizations.{Vectorization, EasyVectorizationLocal}
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices.ExternalsIndicesType
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.vectors.GVector
import shapeless.HMap
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.MetricIDType._
/**
 *
 */
case class ClustersIndicesAnalysisLocal[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
](
    val data: GS[Cz[ID, O, V]],
    val internalsIndicesByMetricClusteringNumberIndex: immutable.Map[(MetricID, ClusteringNumber, InternalsIndicesType), Double] = immutable.Map.empty[(MetricID, ClusteringNumber, InternalsIndicesType), Double]
    // val clusteringInfo: ClusteringInformationsLocal[ID, O, V, EasyVectorizationLocal, Cz, GS] = new ClusteringInformationsLocal[ID, O, V, EasyVectorizationLocal, Cz, GS]
) extends ClustersIndicesAnalysis[ID, O, V, Cz, GS] {
    /**
     *
     */
    type Self = ClustersIndicesAnalysisLocal[ID, O, V, Cz, GS]
    /**
     * Compute given internals indices and add result to internalsIndicesByMetricClusteringNumberIndex
     * @return A Map which link internal indices to its associate value 
     */
    def obtainInternalsIndices[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): immutable.Map[InternalsIndicesType, Double] = {
        val idAndVector: GS[(ClusterID, V)] = data.map( cz => (cz.clusterIDs(clusteringNumber), cz.v) ).asInstanceOf[GS[(ClusterID, V)]]
        val internalIndices = InternalIndicesLocal(idAndVector, metric)

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
     *
     */
    def obtainInternalsIndicesVecto[D <: Distance[V], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](metric: D, indices: InternalsIndicesType*)(vectorization: Vecto[O, V]): immutable.Map[InternalsIndicesType, Double] = {
        obtainInternalsIndices(metric, indices:_*)(vectorization.vectorizationID)
    }
    /**
     *
     */
    def computeInternalsIndicesForSpecificsClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber*): Seq[immutable.Map[InternalsIndicesType, Double]] = {
        clusteringNumber.par.map( cn => obtainInternalsIndices(metric, indices:_*)(cn) ).seq
    }
    /**
     *
     */
    def computeInternalsIndicesForEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): Seq[immutable.Map[InternalsIndicesType, Double]] = {
        computeInternalsIndicesForSpecificsClusteringNumber(metric, indices:_*)((0 until data.head.clusterIDs.size):_*)
    }
    /**
     * Compute given internals indices and add result to internalsIndicesByMetricClusteringNumberIndex
     * @return A Map which link internal indices to its associate value 
     */
    def saveInternalsIndices[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): ClustersIndicesAnalysisLocal[ID, O, V, Cz, GS] = {        
        val idAndVector: GS[(ClusterID, V)] = data.map( cz => (cz.clusterIDs(clusteringNumber), cz.v) ).asInstanceOf[GS[(ClusterID, V)]]
        val obtainedIndices = obtainInternalsIndices(metric, indices:_*)(clusteringNumber).map{ case (indexType, v) => ((metric.id, clusteringNumber, indexType), v) }.seq
        ClustersIndicesAnalysisLocal(data, internalsIndicesByMetricClusteringNumberIndex ++ obtainedIndices)
    }
    /**
     *
     */
    def saveInternalsIndicesForEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): ClustersIndicesAnalysisLocal[ID, O, V, Cz, GS] = {
        val indicess = computeInternalsIndicesForEveryClusteringNumber(metric, indices:_*).zipWithIndex.flatMap{ case (scores, idx) => scores.map{ case (indexType, v) => ((metric.id, idx, indexType), v) } }
        ClustersIndicesAnalysisLocal(data, internalsIndicesByMetricClusteringNumberIndex ++ indicess)
    }
    /**
     * Compute given externals indices and add result to externalsIndicesByClusteringNumber
     * @return A Map which link external indices to its associate value 
     */
    def computeExternalsIndices(groundTruth: GS[ClusterID], indices: ExternalsIndicesType*)(clusteringNumber: ClusteringNumber): immutable.Map[ExternalsIndicesType, Double] = {

        val onlyClusterIDs = data.map(_.clusterIDs(clusteringNumber))

        val obtainedIndices = indices.par.map{ index =>
            index match {
                case MI => (MI, ExternalIndicesLocal.mutualInformation(onlyClusterIDs, groundTruth))
                case NMI_Sqrt => (NMI_Sqrt, ExternalIndicesLocal.nmi(onlyClusterIDs, groundTruth, NmiNormalizationNature.SQRT)) 
                case NMI_Max => (NMI_Max, ExternalIndicesLocal.nmi(onlyClusterIDs, groundTruth, NmiNormalizationNature.MAX))
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
        
        externalsIndicesByClusteringNumber += ((clusteringNumber, obtainedIndices))
        
        obtainedIndices
    }
    /**
     *
     */
    def computeSomeExternalsIndices(groundTruth: GS[ClusterID], clusterIDs: Int*)(indices: ExternalsIndicesType*): Seq[immutable.Map[ExternalsIndicesType, Double]] = {
        clusterIDs.par.map( cn => computeExternalsIndices(groundTruth, indices:_*)(cn) ).seq
    }
    /**
     *
     */
    def computeExternalsIndicesForEveryClusteringNumber(groundTruth: GS[ClusterID], indices: ExternalsIndicesType*): Seq[immutable.Map[ExternalsIndicesType, Double]] = {
        computeSomeExternalsIndices(groundTruth, (0 until data.head.clusterIDs.size):_*)(indices:_*)
    }
}