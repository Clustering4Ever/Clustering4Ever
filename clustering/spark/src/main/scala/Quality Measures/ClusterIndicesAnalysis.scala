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
import org.clustering4ever.indices.{ExternalIndicesDistributed, InternalIndicesDistributed}
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
case class ClustersIndicesAnalysisDistributed[
    O,
    V <: GVector[V] : ClassTag,
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]
](val clusterized: RDD[Cz[O, V]], val sc: SparkContext, persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, val internalsIndicesByMetricClusteringNumberIndex: immutable.Map[(MetricID, ClusteringNumber, InternalsIndicesType), Double] = immutable.Map.empty[(MetricID, ClusteringNumber, InternalsIndicesType), Double]) extends ClustersIndicesAnalysis[O, V, Cz, RDD] {
    /**
     *
     */
    type Self = ClustersIndicesAnalysisDistributed[O, V, Cz]
    /**
     * Compute given internals indices and add result to internalsIndicesByMetricClusteringNumberIndex
     * @return A Map which link internal indices to its associate value 
     */
    def obtainInternalsIndices[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): Map[InternalsIndicesType, Double] = {
        
        val internalIndices = InternalIndicesDistributed(metric)

        indices.map{ index =>
            index match {
                case DaviesBouldin => (DaviesBouldin, internalIndices.daviesBouldin(sc, clusterized, clusteringNumber))
                case BallHall => (BallHall, internalIndices.ballHall(clusterized, clusteringNumber))
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
    }
    /**
     * Compute given internals indices and add result to internalsIndicesByMetricClusteringNumberIndex
     * @return A Map which link internal indices to its associate value 
     */
    def saveInternalsIndices[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): ClustersIndicesAnalysisDistributed[O, V, Cz] = {
        
        val idAndVector: RDD[(ClusterID, V)] = clusterized.map( cz => (cz.clusterIDs(clusteringNumber), cz.v) )

        val obtainedIndices = obtainInternalsIndices(metric, indices:_*)(clusteringNumber).seq.map{ case (indexType, v) => ((metric.id, clusteringNumber, indexType), v) }
        
        ClustersIndicesAnalysisDistributed(clusterized, sc, persistanceLVL, internalsIndicesByMetricClusteringNumberIndex ++ obtainedIndices)
    }
    /**
     *
     */
    def computeInternalsIndicesForEveryClusteringNumber[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*): Seq[Map[InternalsIndicesType, Double]] = {
        (0 until clusterized.first.clusterIDs.size).par.map( cn => obtainInternalsIndices(metric, indices:_*)(cn) ).seq
    }
    /**
     *
     */
    def saveInternalsIndicesForEveryClusteringNumber[D[A <: GVector[A]] <: Distance[A]](metric: D[V], indices: InternalsIndicesType*): ClustersIndicesAnalysisDistributed[O, V, Cz] = {
        val indicess = computeInternalsIndicesForEveryClusteringNumber(metric, indices:_*).zipWithIndex.flatMap{ case (scores, idx) => scores.map{ case (indexType, v) => ((metric.id, idx, indexType), v) } }
        ClustersIndicesAnalysisDistributed(clusterized, sc, persistanceLVL, internalsIndicesByMetricClusteringNumberIndex ++ indicess)
    }
    /**
     * Compute given externals indices and add result to externalsIndicesByClusteringNumber
     * @return A Map which link external indices to its associate value 
     */
    def computeExternalsIndices(groundTruth: RDD[ClusterID], indices: ExternalsIndicesType*)(clusteringNumber: ClusteringNumber = 0): Map[ExternalsIndicesType, Double] = {

        val onlyClusterIDs = groundTruth.zip(clusterized.map(_.clusterIDs(clusteringNumber))).persist(persistanceLVL)

        val obtainedIndices = indices.par.map{ index =>
            index match {
                case MI => (MI, ExternalIndicesDistributed.mutualInformation(sc, onlyClusterIDs))
                case NMI_Sqrt => (NMI_Sqrt, ExternalIndicesDistributed.nmi(sc, onlyClusterIDs, NmiNormalizationNature.SQRT)) 
                case NMI_Max => (NMI_Max, ExternalIndicesDistributed.nmi(sc, onlyClusterIDs, NmiNormalizationNature.MAX))
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
        
        externalsIndicesByClusteringNumber += ((clusteringNumber, obtainedIndices))
        
        obtainedIndices
    }
    /**
     *
     */
    def computeExternalsIndicesForEveryClusteringNumber(groundTruth: RDD[ClusterID], indices: ExternalsIndicesType*): Seq[Map[ExternalsIndicesType, Double]] = {
        (0 until clusterized.first.clusterIDs.size).par.map( cn => computeExternalsIndices(groundTruth, indices:_*)(cn) ).seq
    }
}