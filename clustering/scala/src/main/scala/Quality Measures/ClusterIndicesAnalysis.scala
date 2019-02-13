package org.clustering4ever.clustering.indices
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{GenSeq, immutable, mutable}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.Distance
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
trait ClustersIndicesAnalysisAncestorLocal[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    GS[X] <: GenSeq[X]
] extends ClustersIndicesAnalysis[O, V, Cz, GS] {
    /**
     * Compute given internals indices and add result to internalsIndicesByMetricClusteringNumberIndex
     * @return A Map which link internal indices to its associate value 
     */
    final def obtainInternalsIndices[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): immutable.Map[InternalsIndicesType, Double] = {
        val internalIndices = InternalIndicesLocal(metric)
        indices.par.map{ index =>
            index match {
                case DaviesBouldin => (DaviesBouldin, internalIndices.daviesBouldin(clusterized, clusteringNumber))
                case BallHall => (BallHall, internalIndices.ballHall(clusterized, clusteringNumber))
                case Silhouette => (Silhouette, internalIndices.silhouette(clusterized, clusteringNumber))
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
    }
    /**
     *
     */
    final def obtainInternalsIndicesVecto[D[A <: GVector[A]] <: Distance[A], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]](metric: D[V], indices: InternalsIndicesType*)(vectorization: Vecto[O, V]): immutable.Map[InternalsIndicesType, Double] = {
        obtainInternalsIndices(metric, indices:_*)(vectorization.vectorizationID)
    }
    /**
     *
     */
    final def computeInternalsIndicesForSpecificsClusteringNumber[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber*): Seq[immutable.Map[InternalsIndicesType, Double]] = {
        clusteringNumber.par.map( cn => obtainInternalsIndices(metric, indices:_*)(cn) ).seq
    }
    /**
     *
     */
    final def computeInternalsIndicesForEveryClusteringNumber[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*): Seq[immutable.Map[InternalsIndicesType, Double]] = {
        computeInternalsIndicesForSpecificsClusteringNumber(metric, indices:_*)((0 until clusterized.head.clusterIDs.size):_*)
    }
    /**
     * Compute given externals indices and add result to externalsIndicesByClusteringNumber
     * @return A Map which link external indices to its associate value 
     */
    final def computeExternalsIndices(groundTruth: GS[ClusterID], indices: ExternalsIndicesType*)(clusteringNumber: ClusteringNumber): immutable.Map[ExternalsIndicesType, Double] = {

        val onlyClusterIDs = clusterized.map(_.clusterIDs(clusteringNumber))

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
    final def computeSomeExternalsIndices(groundTruth: GS[ClusterID], clusterIDs: Int*)(indices: ExternalsIndicesType*): Seq[immutable.Map[ExternalsIndicesType, Double]] = {
        clusterIDs.par.map( cn => computeExternalsIndices(groundTruth, indices:_*)(cn) ).seq
    }
    /**
     *
     */
    final def computeExternalsIndicesForEveryClusteringNumber(groundTruth: GS[ClusterID], indices: ExternalsIndicesType*): Seq[immutable.Map[ExternalsIndicesType, Double]] = {
        computeSomeExternalsIndices(groundTruth, (0 until clusterized.head.clusterIDs.size):_*)(indices:_*)
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
](
    final val clusterized: GS[Cz[O, V]],
    final val internalsIndicesByMetricClusteringNumberIndex: immutable.Map[(MetricID, ClusteringNumber, InternalsIndicesType), Double] = immutable.Map.empty[(MetricID, ClusteringNumber, InternalsIndicesType), Double]
    // val clusteringInfo: ClusteringInformationsLocal[ID, O, V, EasyVectorizationLocal, Cz, GS] = new ClusteringInformationsLocal[ID, O, V, EasyVectorizationLocal, Cz, GS]
) extends ClustersIndicesAnalysisAncestorLocal[O, V, Cz, GS] {
    /**
     *
     */
    final type Self = ClustersIndicesAnalysisLocal[O, V, Cz, GS]
    /**
     * Compute given internals indices and add result to internalsIndicesByMetricClusteringNumberIndex
     * @return A Map which link internal indices to its associate value 
     */
    final def saveInternalsIndices[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): ClustersIndicesAnalysisLocal[O, V, Cz, GS] = {        
        val idAndVector: GS[(ClusterID, V)] = clusterized.map( cz => (cz.clusterIDs(clusteringNumber), cz.v) ).asInstanceOf[GS[(ClusterID, V)]]
        val obtainedIndices = obtainInternalsIndices(metric, indices:_*)(clusteringNumber).map{ case (indexType, v) => ((metric.id, clusteringNumber, indexType), v) }.seq
        ClustersIndicesAnalysisLocal(clusterized, internalsIndicesByMetricClusteringNumberIndex ++ obtainedIndices)
    }
    /**
     *
     */
    final def saveInternalsIndicesForEveryClusteringNumber[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*): ClustersIndicesAnalysisLocal[O, V, Cz, GS] = {
        val indicess = computeInternalsIndicesForEveryClusteringNumber(metric, indices:_*).zipWithIndex.flatMap{ case (scores, idx) => scores.map{ case (indexType, v) => ((metric.id, idx, indexType), v) } }
        ClustersIndicesAnalysisLocal(clusterized, internalsIndicesByMetricClusteringNumberIndex ++ indicess)
    }

}