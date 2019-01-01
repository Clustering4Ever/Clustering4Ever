package org.clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.clustering.ClusteringCommons
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.scala.indexes.{ExternalIndexes, InternalIndexes}
import org.clustering4ever.scala.indexes.NmiNormalizationNature
/**
 *
 */
object InternalsIndexes extends Enumeration {
    type InternalsIndexesType = Value
    val DaviesBouldin,
        BallHall,
        Silhouette = Value
}
/**
 *
 */
object ExternalsIndexes extends Enumeration {
    type ExternalsIndexesType = Value
    val MI,
        NMI_Sqrt,
        NMI_Max = Value
}
import InternalsIndexes.InternalsIndexesType
import InternalsIndexes._
import ExternalsIndexes.ExternalsIndexesType
import ExternalsIndexes._
import org.clustering4ever.scala.vectors.GVector
/**
 *
 */
class ClustersIndexesAnalysis[
    ID,
    O,
    V <: GVector,
    Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz]
](clusterized: GenSeq[Cz[ID, O, V]]) extends ClusteringCommons {
    /**
     *
     */
    def computeInternalsIndexes[D <: Distance[V]](metric: D, indexes: InternalsIndexesType*)(workingVector: Int = 0, clusteringNumber: Int = 0): Seq[(InternalsIndexesType, Double)] = {
        
        val idAndVector: GenSeq[(ClusterID, V)] = clusterized.map( cz => (cz.clusterID(clusteringNumber), cz.workingVector) )

        val internalIndexes = new InternalIndexes(idAndVector, metric)

        indexes.par.map{ index =>
            index match {
                case DaviesBouldin => (DaviesBouldin, internalIndexes.daviesBouldin)
                case BallHall => (BallHall, internalIndexes.ballHall)
                case Silhouette => (Silhouette, internalIndexes.silhouette)
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq
    }
    /**
     *
     */
    def computeExternalsIndexes(groundTruth: GenSeq[ClusterID], indexes: ExternalsIndexesType*)(clusteringNumber: Int = 0): Seq[(ExternalsIndexesType, Double)] = {

        val onlyClusterIDs = clusterized.map(_.clusterID(clusteringNumber))

        indexes.par.map{ index =>
            index match {
                case MI => (MI, ExternalIndexes.mutualInformation(onlyClusterIDs, groundTruth))
                case NMI_Sqrt => (NMI_Sqrt, ExternalIndexes.nmi(onlyClusterIDs, groundTruth, NmiNormalizationNature.SQRT)) 
                case NMI_Max => (NMI_Max, ExternalIndexes.nmi(onlyClusterIDs, groundTruth, NmiNormalizationNature.MAX))
                case _ => throw new IllegalArgumentException("Asked index is not repertoried")
            }
        }.seq
    }
}