package org.clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
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
/**
 *
 */
class ClustersIndicesAnalysisDistributed[
    ID,
    O,
    V <: GVector[V] : ClassTag,
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]
](val data: RDD[Cz[ID, O, V]], val sc: SparkContext, persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY) extends ClustersIndicesAnalysis[ID, O, V, Cz, RDD] {
    /**
     * Compute given internals indices and add result to internalsIndicesByClusteringNumber
     * @return A Map which link internal indices to its associate value 
     */
    def computeInternalsIndices[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: Int = 0): Map[InternalsIndicesType, Double] = {
        
        val idAndVector: RDD[(ClusterID, V)] = data.map( cz => (cz.clusterIDs(clusteringNumber), cz.v) ).persist(persistanceLVL)

        val internalIndices = new InternalIndices(idAndVector, metric)

        val obtainedIndices = indices.par.map{ index =>
            index match {
                case DaviesBouldin => (DaviesBouldin, internalIndices.daviesBouldin(sc))
                case BallHall => (BallHall, internalIndices.ballHall)
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq.toMap
        
        internalsIndicesByClusteringNumber += ((clusteringNumber, obtainedIndices))
        
        obtainedIndices
    }
    /**
     *
     */
    def computeInternalsIndicesForEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): Seq[Map[InternalsIndicesType, Double]] = {
        (0 until data.first.clusterIDs.size).par.map( cn => computeInternalsIndices(metric, indices:_*)(cn) ).seq
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