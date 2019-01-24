package org.clustering4ever.clusteranalysis
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
import org.clustering4ever.vectorizations.{Vectorization, VectorizationLocal, EasyVectorizationLocal}
import org.clustering4ever.clustering.{ClustersAnalysis, ClusteringArgs, ClusteringModelLocal, ClusteringInformationsLocal}
/**
 *
 */
trait ClustersAnalysisLocal[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Vecto <: VectorizationLocal[O, V, Vecto],
    GS[X] <: GenSeq[X]
] extends ClustersAnalysis[ID, O, V, Cz, GS] {
    /**
     *
     */
    implicit val ct1: ClassTag[Cz[ID, O, V]]
    /**
     *
     */
    implicit val ct2: ClassTag[V]
    /**
     *
     */
    // val currentVectorization: Vectorization[O, V]
    /**
     *
     */
    val clusteringInformations: HMap[ClusteringInformationsMapping]
    /**
     *
     */
    def getClusterinfInformationsForVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV]): Option[ClusteringInformationsLocal[ID, O, GV, Cz, OtherVecto[O, GV], GS]] = {
        clusteringInformations.get(vectorization.vectorizationID)(ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[ID, O, GV, Cz, OtherVecto[O, GV], GS]])
    }
    /**
     *
     */
    def getClusterinfInformationsForClustering[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]](clusteringNumber: ClusteringNumber, vectorization: OtherVecto[O, GV]): Option[ClusteringInformationsLocal[ID, O, GV, Cz, OtherVecto[O, GV], GS]] = {
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
    def centroids[D <: Distance[V]](metric: D, clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, V] = {
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
    Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto[A, B]],
    GS[X] <: GenSeq[X]
](
    val data: GS[Cz[ID, O, ScalarVector[V]]],
    val currentVectorization: Vecto[O, ScalarVector[V]],
    val vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping],
    val clusteringInformations: HMap[ClusteringInformationsMapping]
)(implicit val ct1: ClassTag[Cz[ID, O, ScalarVector[V]]], val ct2: ClassTag[ScalarVector[V]]) extends ClustersAnalysisLocal[ID, O, ScalarVector[V], Cz, Vecto[O, ScalarVector[V]], GS] {
    /**
     *
     */
    // def centroids[D[X <: Seq[Double]] <: ContinuousDistance[X]](metric: D[V], clusteringNumber: ClusteringNumber): immutable.Map[Int, ScalarVector[V]] = {
    //     val res = groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainMean(aggregate.map(_.v))) }.seq.toMap
    //     centroidsByClusteringNumber += (((clusteringNumber, metric.id), res))
    //     res
    // }
    /**updatedBinaryStats
     *
     */
    def obtainCentroidsMeans(clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, ScalarVector[V]] = {
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
    Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto[A, B]],
    GS[X] <: GenSeq[X]
](  
    val data: GS[Cz[ID, O, BinaryVector[V]]],
    val currentVectorization: Vecto[O, BinaryVector[V]],
    val vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping],
    val clusteringInformations: HMap[ClusteringInformationsMapping]
)(implicit val ct1: ClassTag[Cz[ID, O, BinaryVector[V]]], val ct2: ClassTag[BinaryVector[V]]) extends ClustersAnalysisLocal[ID, O, BinaryVector[V], Cz, Vecto[O, BinaryVector[V]], GS] {
    /**
     *
     */
    val binaryStats = new EveryClusteringBinaryAnalysis
    /**
     *
     */
    private def mergeBinaryStats(bs1: EveryClusteringBinaryAnalysis, bs2: EveryClusteringBinaryAnalysis): EveryClusteringBinaryAnalysis = {
        val keys1 = bs1.byVectorizationByCN.keys.toSet
        val keys2 = bs2.byVectorizationByCN.keys.toSet
        val commonsKeys = (keys1 & keys2).toSeq
        EveryClusteringBinaryAnalysis(immutable.HashMap((bs1.byVectorizationByCN.map{ case (k, m1) => (k, immutable.HashMap((m1 ++ bs2.byVectorizationByCN(k)).toSeq:_*)) } ++ (bs2.byVectorizationByCN -- commonsKeys)).toSeq:_*))

    }
    /**
     *
     */
    def switchToAnotherExistantVector[S <: Seq[Int], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, BinaryVector[S]])(implicit ct: ClassTag[Cz[ID, O, BinaryVector[S]]]) = {
        BinaryClustersAnalysis(
            data.map(_.updateVectorization(vectorization)).asInstanceOf[GS[Cz[ID, O, BinaryVector[S]]]],
            vectorization,
            vectorizations,
            clusteringInformations
        )
    }
    /**
     *
     */
    def obtainCentroidsModes(clusteringNumber: ClusteringNumber): immutable.HashMap[ClusterID, BinaryVector[V]] = {
        immutable.HashMap(groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainMode(aggregate.map(_.v))) }.toSeq.seq:_*)
    }
    /**
     *
     */
    import org.clustering4ever.util.VectorsAddOperationsImplicits._
    /**
     *
     */
    lazy val occurencesPerFeature: Seq[Int] = data.map(_.v.vector).reduce(SumVectors.sumVectors(_, _)) 
    /**
     *
     */
    lazy val frequencyPerFeature: Seq[Double] = occurencesPerFeature.map(_.toDouble / datasetSize)
    /**
     *
    */
    def occurencesPerFeatureByClusteringNumber(clusteringNumber: ClusteringNumber): immutable.HashMap[ClusterID, Seq[Int]] = {
        immutable.HashMap(
            groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) =>
                (
                    clusterID,
                    aggregate.map(_.v).reduce(SumVectors.sumVectors(_, _)).vector
                )
            }.toSeq.seq
        :_*)
    }
    /**
     *
     */
    def frequencyPerFeatureByClusteringNumber(clusteringNumber: ClusteringNumber): (immutable.HashMap[ClusterID, Seq[Int]], immutable.HashMap[ClusterID, Seq[Double]]) = {
        val opfcn = occurencesPerFeatureByClusteringNumber(clusteringNumber)
        val fpfcn = opfcn.map{ case (clusterID, occurences) =>
            (
                clusterID,
                occurences.map(_.toDouble / cardinalitiesByClusteringNumber(clusteringNumber)(clusterID))
            )
        }
        (opfcn, fpfcn)
    }
    /**
     *
     */
    def frequencyPerFeatureEveryMultipleClusteringNumbers(clusteringNumber: ClusteringNumber*): Seq[(ClusteringNumber, (immutable.HashMap[ClusterID, Seq[Int]], immutable.HashMap[ClusterID, Seq[Double]]))] = {
        clusteringNumber.par.map( cn => (cn, frequencyPerFeatureByClusteringNumber(cn)) ).seq
    }
    /**
     *
     */
    def updateBinaryStatsByClusteringNumber(clusteringNumber: ClusteringNumber): BinaryClustersAnalysis[ID, O, V, Cz, Vecto, GS] = {
        val (opfcn, fpfcn) = frequencyPerFeatureByClusteringNumber(clusteringNumber)
        val eca = EveryClusteringBinaryAnalysis(immutable.HashMap(currentVectorization.vectorizationID -> immutable.HashMap(clusteringNumber -> ClusteringBinaryAnalysis(occurencesPerFeature, frequencyPerFeature, opfcn, fpfcn))))
        val updatedBinaryStats = mergeBinaryStats(binaryStats, eca)
        new BinaryClustersAnalysis(data, currentVectorization, vectorizations, clusteringInformations) {
            override val binaryStats = updatedBinaryStats
        }
    }
    /**
     *
     */
    def updateBinaryStatsOnAllClusteringNumberForGivenVectorization[S <: Seq[Int], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, BinaryVector[S]])(implicit ct: ClassTag[Cz[ID, O, BinaryVector[S]]]): BinaryClustersAnalysis[ID, O, S, Cz, OtherVecto, GS] = {
        val requiredVecto = vectorizations.get(vectorization.vectorizationID)(vectorization.vectoMapping).get
        val updatedBinaryClustersAnalysis@BinaryClustersAnalysis(updtData, updtCurrentVectorization, updtVectorizations, updtClusteringInformations) = switchToAnotherExistantVector(vectorization)
        val statsByCn = updatedBinaryClustersAnalysis.frequencyPerFeatureEveryMultipleClusteringNumbers(requiredVecto.clusteringNumbers.toSeq:_*)
        val cbaByCN = immutable.HashMap(statsByCn.map{ case (cn, (opfcn, fpfcn)) => (cn, ClusteringBinaryAnalysis(occurencesPerFeature, frequencyPerFeature, opfcn, fpfcn)) }:_*)

        new BinaryClustersAnalysis(updtData, updtCurrentVectorization, updtVectorizations, updtClusteringInformations) {
            override val binaryStats = EveryClusteringBinaryAnalysis(updatedBinaryClustersAnalysis.binaryStats.byVectorizationByCN + ((currentVectorization.vectorizationID, cbaByCN)))
        }
    }
}