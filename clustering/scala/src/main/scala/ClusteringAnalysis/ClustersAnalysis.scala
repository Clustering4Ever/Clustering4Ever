package org.clustering4ever.clusteringanalysis
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
import org.clustering4ever.clustering.{ClustersAnalysis, ClusteringModel, ClusteringInformationsLocal}
/**
 *
 */
trait ClustersAnalysisLocal[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto],
    GS[X] <: GenSeq[X]
] extends ClustersAnalysis[O, V, Cz, GS] {
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
    def getClusterinfInformationsForVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, GV]): Option[ClusteringInformationsLocal[O, GV, OtherVecto]] = {
        clusteringInformations.get(vectorization.vectorizationID)(ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[O, GV, OtherVecto]])
    }
    /**
     *
     */
    def getClusterinfInformationsForClustering[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](clusteringNumber: ClusteringNumber, vectorization: OtherVecto[O, GV]): Option[ClusteringInformationsLocal[O, GV, OtherVecto]] = {
        getClusterinfInformationsForVectorization(vectorization).find(_.clusteringInformations.exists(_._1 == clusteringNumber))
    }
    /**
     *
     */
    val datasetSize: Long = data.size.toLong
    /**
     *
     */
    var lastGroupedByClusterID: Option[(Int, GenMap[Int, GenSeq[Cz[O, V]]])] = None
    /**
     *
     */
    def groupedByClusterID(clusteringNumber: ClusteringNumber): GenMap[Int, GenSeq[Cz[O, V]]] = {
        if(lastGroupedByClusterID.isDefined && lastGroupedByClusterID.get._1 == clusteringNumber) lastGroupedByClusterID.get._2
        else {
            val groupedByClusterID = data.groupBy(_.clusterIDs(clusteringNumber))
            lastGroupedByClusterID = Some(clusteringNumber, groupedByClusterID)
            groupedByClusterID
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
    def cardinalities(clusteringNumber: ClusteringNumber): immutable.Map[Int, Long] = {
        val cardinalitiesByClusterID = groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, aggregate.size.toLong) }.seq.toMap
        cardinalitiesByClusteringNumber += ((clusteringNumber, cardinalitiesByClusterID))
        cardinalitiesByClusterID
    }
    /**
     *
     */
    def clustersProportions(clusteringNumber: ClusteringNumber): immutable.Map[Int, Double] = {
        val clustersProportionsByClusterID = cardinalities(clusteringNumber).map{ case (clusterID, cardinality) => (clusterID, cardinality.toDouble / datasetSize) }
        clustersProportionsByClusteringNumber += ((clusteringNumber, clustersProportionsByClusterID))
        clustersProportionsByClusterID
    }
}
/**
 * Specific class for real vector datasets
 */
case class RealClustersAnalysis[
    O,
    V <: Seq[Double],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto],
    GS[X] <: GenSeq[X]
](
    val data: GS[Cz[O, ScalarVector[V]]],
    val currentVectorization: Vecto[O, ScalarVector[V]],
    val vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping],
    val clusteringInformations: HMap[ClusteringInformationsMapping]
) extends ClustersAnalysisLocal[O, ScalarVector[V], Cz, Vecto, GS] {
    /**
     *
     */
    // def centroids[D[X <: Seq[Double]] <: ContinuousDistance[X]](metric: D[V], clusteringNumber: ClusteringNumber): immutable.Map[Int, ScalarVector[V]] = {
    //     val res = groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainMean(aggregate.map(_.v))) }.seq.toMap
    //     centroidsByClusteringNumber += (((clusteringNumber, metric.id), res))
    //     res
    // }
    /**
     * @return cluster's means
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
    O,
    V <: Seq[Int],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto],
    GS[X] <: GenSeq[X]
](  
    val data: GS[Cz[O, BinaryVector[V]]],
    val currentVectorization: Vecto[O, BinaryVector[V]],
    val vectorizations: HMap[VectorizationMapping] = HMap.empty[VectorizationMapping],
    val clusteringInformations: HMap[ClusteringInformationsMapping]
) extends ClustersAnalysisLocal[O, BinaryVector[V], Cz, Vecto, GS] {
    /**
     * Field which regroup all analysis made in this class
     */
    val binaryStats = new EveryClusteringBinaryAnalysis
    /**
     *
     */
    private def mergeBinaryStats(bs1: EveryClusteringBinaryAnalysis, bs2: EveryClusteringBinaryAnalysis): EveryClusteringBinaryAnalysis = {
        val keys1 = bs1.clusteringBinaryAnalysisByClusteringNumberByVectorization.keys.toSet
        val keys2 = bs2.clusteringBinaryAnalysisByClusteringNumberByVectorization.keys.toSet
        val commonsKeys = (keys1 & keys2).toSeq
        EveryClusteringBinaryAnalysis(mutable.HashMap((bs1.clusteringBinaryAnalysisByClusteringNumberByVectorization.map{ case (k, m1) => (k, mutable.HashMap((m1 ++ bs2.clusteringBinaryAnalysisByClusteringNumberByVectorization(k)).toSeq:_*)) } ++ (bs2.clusteringBinaryAnalysisByClusteringNumberByVectorization -- commonsKeys)).toSeq:_*))

    }
    /**
     *
     */
    // private def updateBinaryStats(bs1: EveryClusteringBinaryAnalysis): Unit = {
    //     val keysIn = binaryStats.clusteringBinaryAnalysisByClusteringNumberByVectorization.keys.toSet
    //     val keys1 = bs1.clusteringBinaryAnalysisByClusteringNumberByVectorization.keys.toSet
    //     val commonsKeys = (keys1 & keysIn).toSeq
    //     binaryStats.clusteringBinaryAnalysisByClusteringNumberByVectorization ++= 
            
    //         EveryClusteringBinaryAnalysis(
    //             mutable.HashMap(
    //                 (bs1.clusteringBinaryAnalysisByClusteringNumberByVectorization.map{ case (k, m1) => (k, mutable.HashMap((m1 ++ binaryStats.clusteringBinaryAnalysisByClusteringNumberByVectorization(k)).toSeq:_*)) } ++ (binaryStats.clusteringBinaryAnalysisByClusteringNumberByVectorization -- commonsKeys)).toSeq
    //                 :_*)
    //         )
    // }
    /**
     * Switch the working vector for the one given by vectorization
     */
    def switchForExistingVectorization[S <: Seq[Int], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, BinaryVector[S]]) = {
        BinaryClustersAnalysis(
            data.map(_.updateVectorization(vectorization)).asInstanceOf[GS[Cz[O, BinaryVector[S]]]],
            vectorization,
            vectorizations,
            clusteringInformations
        )
    }
    /**
     * @return cluster's modes in the sense of Hamming (majority vote)
     */
    def obtainCentroidsModes(clusteringNumber: ClusteringNumber): immutable.HashMap[ClusterID, BinaryVector[V]] = {
        immutable.HashMap(groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainMode(aggregate.map(_.v))) }.toSeq.seq:_*)
    }
    /**
     *
     */
    import org.clustering4ever.util.VectorsAddOperationsImplicits._
    /**
     * Occurences of every features for the current working vector
     */
    lazy val occurencesPerFeature: Seq[Int] = data.map(_.v.vector).reduce(SumVectors.sumVectors(_, _)) 
    /**
     * Frequencies of every features for the current working vector
     */
    lazy val frequencyPerFeature: Seq[Double] = occurencesPerFeature.map(_.toDouble / datasetSize)
    /**
     * @return occurencesPerFeatureByClusteringNumber
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
     * @return occurencesPerFeatureByClusteringNumber and frequencyPerFeatureByClusteringNumber
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
    def frequencyPerEveryFeatureMultipleClusteringNumbers(clusteringNumber: ClusteringNumber*): Seq[(ClusteringNumber, (immutable.HashMap[ClusterID, Seq[Int]], immutable.HashMap[ClusterID, Seq[Double]]))] = {
        clusteringNumber.par.map( cn => (cn, frequencyPerFeatureByClusteringNumber(cn)) ).seq
    }
    /**
     *
     */
    def updateBinaryStatsByClusteringNumber(clusteringNumber: ClusteringNumber): Unit = {
        val (opfcn, fpfcn) = frequencyPerFeatureByClusteringNumber(clusteringNumber)
        binaryStats.clusteringBinaryAnalysisByClusteringNumberByVectorization(currentVectorization.vectorizationID) += (clusteringNumber -> ClusteringBinaryAnalysis(clusteringNumber, occurencesPerFeature, frequencyPerFeature, opfcn, fpfcn))
    }
    /**
     *
     */
    def updateBinaryStatsOnAllClusteringNumberForGivenVectorization[S <: Seq[Int], OtherVecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, OtherVecto]](vectorization: OtherVecto[O, BinaryVector[S]]): BinaryClustersAnalysis[O, S, Cz, OtherVecto, GS] = {
        val requiredVecto = vectorizations.get(vectorization.vectorizationID)(vectorization.vectoMapping).get
        val updatedBinaryClustersAnalysis@BinaryClustersAnalysis(updtData, updtCurrentVectorization, updtVectorizations, updtClusteringInformations) = switchForExistingVectorization(vectorization)
        val statsByCn = updatedBinaryClustersAnalysis.frequencyPerEveryFeatureMultipleClusteringNumbers(requiredVecto.clusteringNumbers.toSeq:_*)
        val cbaByCN = statsByCn.map{ case (cn, (opfcn, fpfcn)) => (cn, ClusteringBinaryAnalysis(cn, occurencesPerFeature, frequencyPerFeature, opfcn, fpfcn)) }
        
        binaryStats.clusteringBinaryAnalysisByClusteringNumberByVectorization(requiredVecto.vectorizationID) ++= cbaByCN
        val newBinaryStats = binaryStats 

        new BinaryClustersAnalysis(updtData, updtCurrentVectorization, updtVectorizations, updtClusteringInformations) {
            override val binaryStats = newBinaryStats
        }
    }
}