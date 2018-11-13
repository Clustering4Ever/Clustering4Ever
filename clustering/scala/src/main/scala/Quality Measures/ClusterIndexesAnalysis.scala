package clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
import clustering4ever.scala.clusterizables.Clusterizable
import clustering4ever.clustering.ClusteringCommons
import clustering4ever.math.distances.Distance
import clustering4ever.scala.indexes.{ExternalIndexes, InternalIndexes}
import clustering4ever.scala.indexes.NmiNormalizationNature
import clustering4ever.scala.measurableclass.BinaryScalarVector
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
/**
 *
 */
abstract class ClustersIndexesAnalysis[
    @specialized(Int, Long) ID: Numeric,
    O,
    V,
    Cz <: Clusterizable[ID, O, V, Cz]
](clusterized: GenSeq[Cz]) extends ClusteringCommons {
    /**
     *
     */
    def computeExternalsIndexes[D <: Distance[V]](metric: D, indexes: InternalsIndexesType*): Seq[(InternalsIndexesType, Double)] = {
        
        val idAndVector: GenSeq[(ClusterID, V)] = clusterized.map( cz => (cz.clusterID.get, cz.vector) )

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
    def computeInternalsIndexes(groundTruth: GenSeq[ClusterID], indexes: ExternalsIndexesType*): Seq[(ExternalsIndexesType, Double)] = {

        val onlyClusterIDs = clusterized.map(_.clusterID.get)

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