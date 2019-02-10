package org.clustering4ever.clusteringanalysis
/**
 * @author Beck Gaël
 */
import scala.reflect.ClassTag
import scala.collection.{Map, immutable, mutable}
import scala.language.higherKinds
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.util.SumVectors
import org.clustering4ever.clustering.ClustersAnalysis
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
/**
 *
 */
trait ClustersAnalysisDistributed[
    O,
    V <: GVector[V],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]
] extends ClustersAnalysis[O, V, Cz, RDD] {

    private val neutralElement = mutable.ArrayBuffer.empty[Cz[O, V]]
    def addToBuffer(buff: mutable.ArrayBuffer[Cz[O, V]], elem: Cz[O, V]) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[Cz[O, V]], buff2: mutable.ArrayBuffer[Cz[O, V]]) = buff1 ++= buff2
    /**
     *
     */
    lazy val datasetSize = data.count
    /**
     *
     */
    def groupedByClusterID(clusteringNumber: ClusteringNumber)(implicit ct: ClassTag[Cz[O, V]]): RDD[(ClusterID, mutable.ArrayBuffer[Cz[O, V]])] = {
        data.map( cz => (cz.clusterIDs(clusteringNumber), cz) ).aggregateByKey(neutralElement)(addToBuffer, aggregateBuff)
    }
    /**
     *
     */
    def cardinalities(clusteringNumber: ClusteringNumber)(implicit ct: ClassTag[Cz[O, V]]): immutable.Map[ClusterID, Int] = {
        groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, aggregate.size) }.collect.toMap
    }
    /**
     *
     */
    // def clustersProportions(clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, Double] = {
        // cardinalitiesByClusteringNumber(clusteringNumber).map{ case (clusterID, cardinality) => (clusterID, cardinality.toDouble / datasetSize) }
    // }
    /**
     *
     */
    def centroids[D[X <: GVector[X]] <: Distance[X]](metric: D[V], clusteringNumber: ClusteringNumber)(implicit ct: ClassTag[Cz[O, V]], ct2: ClassTag[V]): immutable.Map[ClusterID, V] = {
        groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.v), metric)) }.collect.toMap
    }
    // def cardinalities(clusteringNumber: Int): Map[Int, Long]

    // val cardinalitiesByClusteringNumber = mutable.HashMap.empty[Int, Map[Int, Long]]

    // def clustersProportions(clusteringNumber: Int): Map[Int, Double]

    // val clustersProportionsByClusteringNumber = mutable.HashMap.empty[Int, Map[Int, Double]]

    // val metric: D    

    // def centroids(clusteringNumber: Int): Map[Int, V]

    // val centroidsByClusteringNumber = mutable.HashMap.empty[Int, Map[Int, V]]

}
/**
 *
 */
// class RealClustersAnalysis[
//     ID,
//     O,
//     V <: Seq[Double],
//     Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
//     D <: ContinuousDistance[V]
// ](clusterized: RDD[Cz[O, ScalarVector[V]]], metric: D)(implicit ct: ClassTag[Cz[O, ScalarVector[V]]], ct2: ClassTag[V]) extends ClustersAnalysisDistributed[ID, O, ScalarVector[V], Cz, D](clusterized, metric) {

//     /**
//      * TO DO
//      * - distributions of each features
//      */

// }
/**
 *
 */
// class BinaryClustersAnalysis[
//     ID,
//     O,
//     V <: Seq[Int],
//     Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
//     D <: BinaryDistance[V]
// ](clusterized: RDD[Cz[O, BinaryVector[V]]], metric: D, vectorHeader: Option[mutable.ArrayBuffer[String]] = None, eachCategoryRange: Option[mutable.ArrayBuffer[Int]] = None)(implicit ct: ClassTag[Cz[O, BinaryVector[V]]], ct2: ClassTag[V]) extends ClustersAnalysisDistributed[ID, O, BinaryVector[V], Cz, D](clusterized, metric) {

//     private val originalVector = 0

//     import org.clustering4ever.util.VectorsAddOperationsImplicits._

//     if(vectorHeader.isDefined) require(clusterized.first.workingVector.vector.size == vectorHeader.size)

//     lazy val occurencesPerFeature: V = clusterized.map(_.workingVector.vector).reduce(SumVectors.sumVectors(_, _))

//     lazy val frequencyPerFeature: Seq[Double] = occurencesPerFeature.map(_.toDouble / datasetSize)

//     def occurencesPerFeaturePerCluster(clusteringNumber: Int) = groupedByClusterID(clusteringNumber).map{ case (clusterID, aggregate) => (clusterID, aggregate.map(_.workingVector.vector).reduce(SumVectors.sumVectors(_, _))) }.collectAsMap

//     def frequencyPerFeaturePerCluster(clusteringNumber: Int) = occurencesPerFeaturePerCluster(clusteringNumber).map{ case (clusterID, occurences) => (clusterID, occurences.map(_.toDouble / cardinalities(clusterID))) }
// }