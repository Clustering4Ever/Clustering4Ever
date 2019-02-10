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
import org.clustering4ever.vectors.{GVector, BinaryVector, ScalarVector, GMixedVector}
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.vectorizations.{Vectorization, VectorizationGenLocal, VectorizationLocalBinary, VectorizationLocal, EasyVectorizationLocal}
import org.clustering4ever.clustering.{ClusteringBasicStatsKeeper, ClustersAnalysis, ClusteringModel, ClusteringInformationsLocal}
/**
 *
 */
trait ClustersAnalysisLocal[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Vecto <: VectorizationGenLocal[O, V, Vecto],
    GS[X] <: GenSeq[X]
] extends ClustersAnalysis[O, V, Cz, GS] {
    /**
     *
     */
    val vectorization: Vecto
    /**
     *
     */
    protected val allClusteringNumbers: Seq[Int] = vectorization.runnedAlgorithms.map(_._1)
    /**
     *
     */
    // val clusteringInformations: HMap[ClusteringInformationsMapping]
    /**
     *
     */
    // def getClusterinfInformationsForVectorization[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationGenLocal[A, B, OtherVecto[A, B]]](vectorization: OtherVecto[O, GV]): Option[ClusteringInformationsLocal[O, GV, OtherVecto]] = {
    //     clusteringInformations.get(vectorization.vectorizationID)(ClusteringInformationsMapping[VectorizationID, ClusteringInformationsLocal[O, GV, OtherVecto]])
    // }
    /**
     *
     */
    // def getClusterinfInformationsForClustering[GV <: GVector[GV], OtherVecto[A, B <: GVector[B]] <: VectorizationGenLocal[A, B, OtherVecto[A, B]]](clusteringNumber: ClusteringNumber, vectorization: OtherVecto[O, GV]): Option[ClusteringInformationsLocal[O, GV, OtherVecto]] = {
    //     getClusterinfInformationsForVectorization(vectorization).find(_.clusteringInformations.exists(_._1 == clusteringNumber))
    // }
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

    def centroids[D <: Distance[V]](metric: D, clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, V] = {
        val centroids = groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.v), metric)) }.seq.toMap
        clustersBasicStatsKeeper.addCentroids(clusteringNumber, metric.id, centroids)
        centroids
    }

    def cardinalities(clusteringNumber: ClusteringNumber): immutable.Map[Int, Long] = {
        val cardinalitiesByClusterID = groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, aggregate.size.toLong) }.seq.toMap
        clustersBasicStatsKeeper.addCardinalities(clusteringNumber, cardinalitiesByClusterID)
        cardinalitiesByClusterID
    }

    def clustersProportions(clusteringNumber: ClusteringNumber): immutable.Map[Int, Double] = {
        val clustersProportionsByClusterID = cardinalities(clusteringNumber).map{ case (clusterID, cardinality) => (clusterID, cardinality.toDouble / datasetSize) }
        clustersBasicStatsKeeper.addClustersProportions(clusteringNumber, clustersProportionsByClusterID)
        clustersProportionsByClusterID
    }
    /**
     *
     */
    def centroidsForEveryClusteringNumber[D <: Distance[V]](metric: D): Unit = allClusteringNumbers.par.foreach(centroids(metric, _))
    /**
     *
     */
    def cardinalitiesForEveryClusteringNumber: Unit = allClusteringNumbers.par.foreach(cardinalities(_))
    /**
     *
     */
    def clustersProportionsForEveryClusteringNumber: Unit = allClusteringNumbers.par.foreach(clustersProportions(_))
}
/**
 * Specific class for real vector datasets
 */
case class RealClustersAnalysis[
    O,
    V <: Seq[Double],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Vecto[A, B <: GVector[B]] <: VectorizationGenLocal[A, B, Vecto[A, B]],
    GS[X] <: GenSeq[X]
](
    val data: GS[Cz[O, ScalarVector[V]]],
    val vectorization: Vecto[O, ScalarVector[V]],
    val clustersBasicStatsKeeper: ClusteringBasicStatsKeeper[ScalarVector[V]] = new ClusteringBasicStatsKeeper[ScalarVector[V]]
) extends ClustersAnalysisLocal[O, ScalarVector[V], Cz, Vecto[O, ScalarVector[V]], GS] {
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
    Vecto[A, B <: Seq[Int]] <: VectorizationLocalBinary[A, B, Vecto[A, B]],
    GS[X] <: GenSeq[X]
](  
    val data: GS[Cz[O, BinaryVector[V]]],
    val vectorization: Vecto[O, V],
    val clustersBasicStatsKeeper: ClusteringBasicStatsKeeper[BinaryVector[V]] = new ClusteringBasicStatsKeeper[BinaryVector[V]],
    val advancedBinaryStats: EveryClusteringBinaryAnalysis = new EveryClusteringBinaryAnalysis
) extends ClustersAnalysisLocal[O, BinaryVector[V], Cz, Vecto[O, V], GS] {
    /**
     * Switch the working vector for the one given by vectorization
     */
    def switchForSameVectorizationNature(newVectorization: Vecto[O, V]) = {
        BinaryClustersAnalysis(
            data.map(_.updateVectorizationOfSameNature(newVectorization)).asInstanceOf[GS[Cz[O, BinaryVector[V]]]],
            newVectorization,
            clustersBasicStatsKeeper,
            advancedBinaryStats
        )
    }
    /**
     * @return cluster's modes in the sense of Hamming (majority vote)
     */
    def obtainCentroidsModes(clusteringNumber: ClusteringNumber): immutable.HashMap[ClusterID, BinaryVector[V]] = {
        immutable.HashMap(groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainMode(aggregate.map(_.v))) }.toSeq.seq:_*)
    }
    /**
     * Occurences of every features for the current working vector
     */
    lazy val occurencesPerFeature: Seq[Int] = {
        import org.clustering4ever.util.VectorsAddOperationsImplicits._
        data.map(_.v.vector).reduce(SumVectors.sumVectors(_, _)) 
    }
    /**
     * Frequencies of every features for the current working vector
     */
    lazy val frequencyPerFeature: Seq[Double] = occurencesPerFeature.map(_.toDouble / datasetSize)
    /**
     * @return occurencesPerFeatureByClusteringNumber
     */
    def occurencesPerFeatureByClusteringNumber(clusteringNumber: ClusteringNumber): immutable.HashMap[ClusterID, Seq[Int]] = {
        import org.clustering4ever.util.VectorsAddOperationsImplicits._
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
                occurences.map(_.toDouble / clustersBasicStatsKeeper.cardinalitiesByClusteringNumber.getOrElse(clusteringNumber, cardinalities(clusteringNumber))(clusterID))
            )
        }
        (opfcn, fpfcn)
    }
    /**
     *
     */
    def updateBinaryStatsByClusteringNumber(clusteringNumber: ClusteringNumber): Unit = {
        val (opfcn, fpfcn) = frequencyPerFeatureByClusteringNumber(clusteringNumber)
        val cba = ClusteringBinaryAnalysis(clusteringNumber, occurencesPerFeature, frequencyPerFeature, opfcn, fpfcn)
        advancedBinaryStats.addCBA(vectorization.vectorizationID, clusteringNumber, cba)
    }
    /**
     *
     */
    def everyClusteringFrequencyPerEveryFeature: Seq[(ClusteringNumber, (immutable.HashMap[ClusterID, Seq[Int]], immutable.HashMap[ClusterID, Seq[Double]]))] = {
        allClusteringNumbers.par.map( cn => (cn, frequencyPerFeatureByClusteringNumber(cn)) ).seq
    }
    /**
     * Update AdvancedBinaryStats internal object for every clusteringNumber corresponding to associate vectorization
     */
    def everyClusteringUpdateBinaryStats: Unit = allClusteringNumbers.par.foreach(updateBinaryStatsByClusteringNumber(_))
    /**
     * @return cluster's modes in the sense of Hamming (majority vote) for every clustering corresponding to the given vectorization
     */
    def everyClusteringObtainAllCentroidsModes: immutable.Map[ClusteringNumber, immutable.HashMap[ClusterID, BinaryVector[V]]] = allClusteringNumbers.par.map( cn => (cn, obtainCentroidsModes(cn)) ).seq.toMap

}
/**
 *
 */
object BinaryClustersAnalysis extends Serializable {
    /**
     *
     */
    def obtainManyStats[
        O,
        V <: Seq[Int],
        Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
        Vecto[A, B <: Seq[Int]] <: VectorizationLocalBinary[A, B, Vecto[A, B]],
        GS[X] <: GenSeq[X]
    ](data: GS[Cz[O, BinaryVector[V]]], vectorization: Vecto[O, V]): (ClusteringBasicStatsKeeper[BinaryVector[V]], EveryClusteringBinaryAnalysis) = {
        val bca = BinaryClustersAnalysis(data, vectorization)
        bca.cardinalitiesForEveryClusteringNumber
        bca.clustersProportionsForEveryClusteringNumber
        bca.everyClusteringUpdateBinaryStats
        (
            bca.clustersBasicStatsKeeper,
            bca.advancedBinaryStats
        )
    }
    /**
     *
     */
    def obtainManyStats[
        O,
        V <: Seq[Int],
        Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
        Vecto[A, B <: Seq[Int]] <: VectorizationLocalBinary[A, B, Vecto[A, B]],
        GS[X] <: GenSeq[X]
    ](data: GS[Cz[O, BinaryVector[V]]], vectorization: Vecto[O, V], clustersBasicStatsKeeper: ClusteringBasicStatsKeeper[BinaryVector[V]] = new ClusteringBasicStatsKeeper[BinaryVector[V]], advancedBinaryStats: EveryClusteringBinaryAnalysis = new EveryClusteringBinaryAnalysis): (ClusteringBasicStatsKeeper[BinaryVector[V]], EveryClusteringBinaryAnalysis) = {
        val bca = BinaryClustersAnalysis(data, vectorization, clustersBasicStatsKeeper, advancedBinaryStats)
        bca.cardinalitiesForEveryClusteringNumber
        bca.clustersProportionsForEveryClusteringNumber
        bca.everyClusteringUpdateBinaryStats
        (
            bca.clustersBasicStatsKeeper,
            bca.advancedBinaryStats
        )
    }

}