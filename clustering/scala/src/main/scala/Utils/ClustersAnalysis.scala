package org.clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.existentials
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{Map, GenMap, mutable, immutable, GenSeq}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.shapeless.VMapping
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.util.SumVectors
import org.clustering4ever.vectors.{GVector, BinaryVector, ScalarVector, GMixtVector}
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.vectorizations.{Vectorization, EasyVectorization}
import org.clustering4ever.clustering.{ClustersAnalysis, ClusteringArgs, ClusteringModelLocal, ClusteringInformationsLocal}
/**
 *
 */
trait ClustersAnalysisLocal[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
] extends ClustersAnalysis[ID, O, V, Cz, GS] {
    /**
     *
     */
    def extractClusteringInformationsForSpecificVectorizationAndCastThemUntilIFindSomethingMoreElegant[NV <: Seq[Int], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, BinaryVector[NV]]): immutable.Vector[(ClusteringNumber, Vecto[O, BinaryVector[NV]], ClusteringArgs[BinaryVector[NV]], ClusteringModelLocal[ID, O, BinaryVector[NV], Cz, GS, ClusteringArgs[BinaryVector[NV]]])] = {
        clusteringInfo.clusteringInformations.filter{ case (clusteringNumber, vectorizationIn, _, _) => vectorization.clusteringNumbers.contains(clusteringNumber) }
            .asInstanceOf[immutable.Vector[(ClusteringNumber, Vecto[O, BinaryVector[NV]], ClusteringArgs[BinaryVector[NV]], ClusteringModelLocal[ID, O, BinaryVector[NV], Cz, GS, ClusteringArgs[BinaryVector[NV]]])]]
    }
    /**
     *
     */
    def extractClusteringInformationsForSpecificClusteringNumberAndCastThemUntilIFindSomethingMoreElegant[NV <: GVector[NV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](clusteringNumber: ClusteringNumber, vecto: Vecto[O, NV]) = {
        require(vecto.clusteringNumbers.contains(clusteringNumber))
        clusteringInfo.clusteringInformations(clusteringNumber).asInstanceOf[(Int, Vecto[O, NV], ClusteringArgs[NV], ClusteringModelLocal[ID, O, NV, Cz, GS, ClusteringArgs[NV]])]
    }
    /**
     *
     */
    val clusteringInfo: ClusteringInformationsLocal[ID, O, Cz, GS] = new ClusteringInformationsLocal[ID, O, Cz, GS]
    /**
     *
     */
    val datasetSize: Long = data.size.toLong
    /**
     *
     */
    def groupedByClusterID(clusteringNumber: ClusteringNumber): GenMap[Int, GenSeq[Cz[ID, O, V]]] = {
        if(lastGroupedByClusterID.isDefined && lastGroupedByClusterID.get._1 == clusteringNumber) lastGroupedByClusterID.get._2
        else {
            val res = data.groupBy(_.clusterIDs(clusteringNumber))
            lastGroupedByClusterID = Some(clusteringNumber, res)
            res
        }
    }
    /**
     *
     */
    var lastGroupedByClusterID: Option[(Int, GenMap[Int, GenSeq[Cz[ID, O, V]]])] = None
    /**
     *
     */
    def cardinalities(clusteringNumber: ClusteringNumber): immutable.Map[Int, Long] = {
        val res = groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, aggregate.size.toLong) }.seq.toMap
        cardinalitiesByClusteringNumber += ((clusteringNumber, res))
        res
    }
    /**
     *
     */
    def clustersProportions(clusteringNumber: ClusteringNumber): immutable.Map[Int, Double] = {
        val res = cardinalities(clusteringNumber).map{ case (clusterID, cardinality) => (clusterID, cardinality.toDouble / datasetSize) }
        clustersProportionsByClusteringNumber += ((clusteringNumber, res))
        res
    }
    /**
     *
     */
    def centroids[D <: DistanceRestriction](metric: D, clusteringNumber: ClusteringNumber): immutable.Map[Int, V] = {
        val res = groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.v), metric)) }.seq.toMap
        centroidsByClusteringNumber += (((clusteringNumber, metric.id), res))
        res
    }
}
/**
 * Specific class for real vector datasets
 */
class RealClustersAnalysis[
    ID,
    O,
    V <: Seq[Double],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
](val data: GS[Cz[ID, O, ScalarVector[V]]])(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]) extends ClustersAnalysisLocal[ID, O, ScalarVector[V], Cz, GS] {
    /**
     *
     */
    type DistanceRestriction <: ContinuousDistance[V]
    /**
     * TO DO
     * - distributions of each features
     */

}
/**
 * Specific class for binary vector datasets
 */
class BinaryClustersAnalysis[
    ID,
    O,
    V <: Seq[Int],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
](val data: GS[Cz[ID, O, BinaryVector[V]]])(implicit ct: ClassTag[Cz[ID, O, BinaryVector[V]]]) extends ClustersAnalysisLocal[ID, O, BinaryVector[V], Cz, GS] {
    /**
     *
     */
    type DistanceRestriction <: BinaryDistance[V]
    /**
     *
     */
    import org.clustering4ever.util.VectorsAddOperationsImplicits._
    /**
     *
     */
    lazy val occurencesPerFeature: V = data.map(_.v.vector).reduce(SumVectors.sumVectors(_, _)) 
    /**
     *
     */
    lazy val frequencyPerFeature: Seq[Double] = occurencesPerFeature.map(_.toDouble / datasetSize)
    /**
     *
    */
    def occurencesPerFeaturePerCluster(clusteringNumber: ClusteringNumber): GenMap[Int, V] = {
        groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) =>
            (
                clusterID,
                aggregate.map(_.v).reduce(SumVectors.sumVectors(_, _)).vector
            )
        }.toMap
    }
    /**
     *
     */
    def frequencyPerFeaturePerCluster(clusteringNumber: ClusteringNumber): GenMap[Int, Seq[Double]] = {
        occurencesPerFeaturePerCluster(clusteringNumber).map{ case (clusterID, occurences) =>
            (
                clusterID,
                occurences.map(_.toDouble / cardinalitiesByClusteringNumber(clusteringNumber)(clusterID))
            )
        }
    }
}