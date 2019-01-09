package org.clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{immutable, mutable}
import org.apache.spark.rdd.RDD
import org.apache.spark.SparkContext
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.ClustersIndicesAnalysis
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.spark.indices.{ExternalIndices, InternalIndices}
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
class ClustersIndicesAnalysisDistributed[
    ID,
    O,
    V <: GVector[V] : ClassTag,
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]
](val data: RDD[Cz[ID, O, V]], val sc: SparkContext, persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY, override val internalsIndicesByClusteringNumber: mutable.HashMap[(Int, Int, InternalsIndicesType), Double] = mutable.HashMap.empty[(Int, Int, InternalsIndicesType), Double]) extends ClustersIndicesAnalysis[ID, O, V, Cz, RDD] {
    /**
     *
     */
    type Self = ClustersIndicesAnalysisDistributed[ID, O, V, Cz]
    /**
     * Compute given internals indices and add result to internalsIndicesByClusteringNumber
     * @return A Map which link internal indices to its associate value 
     */
    def obtainInternalsIndices[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: Int = 0): Map[InternalsIndicesType, Double] = {
        
        val idAndVector: RDD[(ClusterID, V)] = data.map( cz => (cz.clusterIDs(clusteringNumber), cz.v) ).persist(persistanceLVL)

        val internalIndices = new InternalIndices(idAndVector, metric)

        indices.map{ index =>
            index match {
                case DaviesBouldin => (DaviesBouldin, internalIndices.daviesBouldin(sc))
                case BallHall => (BallHall, internalIndices.ballHall)
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
    }
    /**
     * Compute given internals indices and add result to internalsIndicesByClusteringNumber
     * @return A Map which link internal indices to its associate value 
     */
    def saveInternalsIndices[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: Int = 0): ClustersIndicesAnalysisDistributed[ID, O, V, Cz] = {
        
        val idAndVector: RDD[(ClusterID, V)] = data.map( cz => (cz.clusterIDs(clusteringNumber), cz.v) )

        val obtainedIndices = obtainInternalsIndices(metric, indices:_*)(clusteringNumber).seq.map{ case (indexType, v) => ((metric.id, clusteringNumber, indexType), v) }
        
        new ClustersIndicesAnalysisDistributed(data, sc, persistanceLVL, internalsIndicesByClusteringNumber ++= obtainedIndices)
    }
    /**
     *
     */
    def computeInternalsIndicesForEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): Seq[Map[InternalsIndicesType, Double]] = {
        (0 until data.first.clusterIDs.size).par.map( cn => obtainInternalsIndices(metric, indices:_*)(cn) ).seq
    }
    /**
     *
     */
    def saveInternalsIndicesForEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): ClustersIndicesAnalysisDistributed[ID, O, V, Cz] = {
        
        val indicess = computeInternalsIndicesForEveryClusteringNumber(metric, indices:_*).zipWithIndex.flatMap{ case (scores, idx) => scores.map{ case (indexType, v) => ((metric.id, idx, indexType), v) } }

        new ClustersIndicesAnalysisDistributed(data, sc, persistanceLVL, internalsIndicesByClusteringNumber ++= indicess)
    }

    /**
     * Compute given externals indices and add result to externalsIndicesByClusteringNumber
     * @return A Map which link external indices to its associate value 
     */
    def computeExternalsIndices(groundTruth: RDD[ClusterID], indices: ExternalsIndicesType*)(clusteringNumber: Int = 0): Map[ExternalsIndicesType, Double] = {

        val onlyClusterIDs = groundTruth.zip(data.map(_.clusterIDs(clusteringNumber))).persist(persistanceLVL)

        val obtainedIndices = indices.par.map{ index =>
            index match {
                case MI => (MI, ExternalIndices.mutualInformation(sc, onlyClusterIDs))
                case NMI_Sqrt => (NMI_Sqrt, ExternalIndices.nmi(sc, onlyClusterIDs, NmiNormalizationNature.SQRT)) 
                case NMI_Max => (NMI_Max, ExternalIndices.nmi(sc, onlyClusterIDs, NmiNormalizationNature.MAX))
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
        (0 until data.first.clusterIDs.size).par.map( cn => computeExternalsIndices(groundTruth, indices:_*)(cn) ).seq
    }
}