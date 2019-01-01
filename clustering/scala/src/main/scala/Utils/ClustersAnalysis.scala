package org.clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{Map, GenMap, mutable, GenSeq}
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.clustering.ClustersAnalysis
import org.clustering4ever.shapelesslinked.VMapping
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.util.SumVectors
import org.clustering4ever.scala.indexes.{ExternalIndexes, InternalIndexes}
import org.clustering4ever.scala.vectors.{GVector, BinaryVector, ScalarVector, GMixtVector}
/**
 *
 */
trait ClustersAnalysisLocal[
    ID,
    O,
    V <: GVector,
    Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
    D <: Distance[V],
    GS[X] <: GenSeq[X]
] extends ClustersAnalysis[ID, O, V, Cz, D, GS] {
    /**
     *
     */
    val datasetSize: Long = data.size.toLong
    /**
     *
     */
    def groupedByClusterID(clusteringNumber: Int): GenMap[Int, GenSeq[Cz[ID, O, V]]] = {
        if(lastGroupedByClusterID.isDefined && lastGroupedByClusterID.get._1 == clusteringNumber) lastGroupedByClusterID.get._2
        else {
            val res = data.groupBy(_.clusterID(clusteringNumber))
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
    def cardinalities(clusteringNumber: Int): Map[Int, Long] = {
        val res = groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, aggregate.size.toLong) }.seq
        cardinalitiesByClusteringNumber += ((clusteringNumber, res))
        res
    }
    /**
     *
     */
    def clustersProportions(clusteringNumber: Int): Map[Int, Double] = {
        val res = cardinalities(clusteringNumber).map{ case (clusterID, cardinality) => (clusterID, cardinality.toDouble / datasetSize) }
        clustersProportionsByClusteringNumber += ((clusteringNumber, res))
        res
    }
    /**
     *
     */
    def centroids(clusteringNumber: Int): Map[Int, V] = {
        val res = groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.workingVector), metric)) }.seq
        centroidsByClusteringNumber += ((clusteringNumber, res))
        res
    }
}
/**
 *
 */
class RealClustersAnalysis[
    ID,
    O,
    V <: Seq[Double],
    Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
    D <: ContinuousDistance[V],
    GS[X] <: GenSeq[X]
](val data: GS[Cz[ID, O, ScalarVector[V]]], val metric: D)(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]) extends ClustersAnalysisLocal[ID, O, ScalarVector[V], Cz, D, GS] {

    /**
     * TO DO
     * - distributions of each features
     */

}
/**
 *
 */
class BinaryClustersAnalysis[
    ID,
    O,
    V <: Seq[Int],
    Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
    D <: BinaryDistance[V],
    GS[X] <: GenSeq[X]
](val data: GS[Cz[ID, O, BinaryVector[V]]], val metric: D, vectorHeader: Option[mutable.ArrayBuffer[String]] = None, eachCategoryRange: Option[mutable.ArrayBuffer[Int]] = None)(implicit ct: ClassTag[Cz[ID, O, BinaryVector[V]]]) extends ClustersAnalysisLocal[ID, O, BinaryVector[V], Cz, D, GS] {

    // private val originalVector = 0

    // import org.clustering4ever.util.VectorsAddOperationsImplicits._

    // if(vectorHeader.isDefined) require(data.head.workingVector.vector.size == vectorHeader.size)

    // lazy val occurencesPerFeature: V = data.map(_.workingVector.vector).reduce(SumVectors.sumVectors(_, _))

    // lazy val frequencyPerFeature: Seq[Double] = occurencesPerFeature.map(_.toDouble / datasetSize)

    // lazy val occurencesPerFeaturePerCluster = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, aggregate.map(_.workingVector).reduce(SumVectors.sumVectors(_, _))) }.toMap

    // lazy val frequencyPerFeaturePerCluster = occurencesPerFeaturePerCluster.map{ case (clusterID, occurences) => (clusterID, occurences.vector.map(_.toDouble / cardinalities(clusterID))) }
}