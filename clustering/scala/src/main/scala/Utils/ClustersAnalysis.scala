package org.clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.existentials
import scala.language.higherKinds
import scala.reflect.ClassTag
import shapeless.HMap
import scala.collection.{Map, GenMap, mutable, immutable, GenSeq}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.shapeless.{VMapping, ClusteringInformationsMapping, VectorizationMapping}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.util.SumVectors
import org.clustering4ever.vectors.{GVector, BinaryVector, ScalarVector, GMixtVector}
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.vectorizations.{Vectorization, EasyVectorizationLocal}
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

    implicit val ct1: ClassTag[Cz[ID, O, V]]
    implicit val ct2: ClassTag[V]
    /**
     *
     */
    // val currentVectorization: Vectorization[O, V]
    /**
     *
     */
    val clusteringInformations: HMap[ClusteringInformationsMapping] = HMap.empty[ClusteringInformationsMapping]
    /**
     *
     */
    def getClusterinfInformationsForVectorization[NV <: GVector[NV]](vectorization: EasyVectorizationLocal[O, NV]): Option[ClusteringInformationsLocal[ID, O, NV, Cz, EasyVectorizationLocal[O, NV], GS]] = {
        clusteringInformations.get(vectorization.vectorizationID)(ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[ID, O, NV, Cz, EasyVectorizationLocal[O, NV], GS]])
    }
    /**
     *
     */
    def getClusterinfInformationsForClustering[NV <: GVector[NV]](clusteringNumber: ClusteringNumber, vectorization: EasyVectorizationLocal[O, NV]): Option[ClusteringInformationsLocal[ID, O, NV, Cz, EasyVectorizationLocal[O, NV], GS]] = {
        getClusterinfInformationsForVectorization(vectorization).find(_.clusteringInformations.exists(_._1 == clusteringNumber))
    }
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
    def centroids[D[X <: GVector[X]] <: Distance[X]](metric: D[V], clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, V] = {
        groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.v), metric)) }.seq.toMap
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
}
/**
 * Specific class for real vector datasets
 */
case class RealClustersAnalysis[
    ID,
    O,
    V <: Seq[Double],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
](
    val data: GS[Cz[ID, O, ScalarVector[V]]],
    val currentVectorization: EasyVectorizationLocal[O, ScalarVector[V]],
    val vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping]
)(implicit val ct1: ClassTag[Cz[ID, O, ScalarVector[V]]], val ct2: ClassTag[ScalarVector[V]]) extends ClustersAnalysisLocal[ID, O, ScalarVector[V], Cz, GS] {
    /**
     *
     */
    // def centroids[D[X <: Seq[Double]] <: ContinuousDistance[X]](metric: D[V], clusteringNumber: ClusteringNumber): immutable.Map[Int, ScalarVector[V]] = {
    //     val res = groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainMean(aggregate.map(_.v))) }.seq.toMap
    //     centroidsByClusteringNumber += (((clusteringNumber, metric.id), res))
    //     res
    // }
    /**
     *
     */
    def obtainCentroidsCenter[D[X <: Seq[Double]] <: ContinuousDistance[X]](clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, ScalarVector[V]] = {
        groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainMean(aggregate.map(_.v))) }.seq.toMap
    }
    /**
     *
     */
    /**
     * TO DO
     * - distributions of each features
     */

}
/**
 * Specific class for binary vector datasets
 */
case class BinaryClustersAnalysis[
    ID,
    O,
    V <: Seq[Int],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    GS[X] <: GenSeq[X]
](  
    val data: GS[Cz[ID, O, BinaryVector[V]]],
    val currentVectorization: EasyVectorizationLocal[O, BinaryVector[V]] = EasyVectorizationLocal[O, BinaryVector[V]](0),
    val vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping]
)(implicit val ct1: ClassTag[Cz[ID, O, BinaryVector[V]]], val ct2: ClassTag[BinaryVector[V]]) extends ClustersAnalysisLocal[ID, O, BinaryVector[V], Cz, GS] {
    /**
     *
     */
    /**
     *
     */
    def updateVectorization[NV <: Seq[Int]](vectorization: EasyVectorizationLocal[O, BinaryVector[NV]])(implicit ct: ClassTag[Cz[ID, O, BinaryVector[NV]]]) = {
        BinaryClustersAnalysis(
            data.map(_.updateVectorization(vectorization)).asInstanceOf[GS[Cz[ID, O, BinaryVector[NV]]]],
            vectorization,
            vectorizations
        )
    }
    /**
     *
     */
    def obtainCentroidsCenter[D[X <: Seq[Int]] <: BinaryDistance[X]](clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, BinaryVector[V]] = {
        groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainMode(aggregate.map(_.v))) }.seq.toMap
    }
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