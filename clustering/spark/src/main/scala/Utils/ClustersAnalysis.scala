package org.clustering4ever.spark.clustersanalysis
/**
 * @author Beck Gaël
 */
import scala.reflect.ClassTag
import scala.collection.{Map, mutable}
import scala.language.higherKinds
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.math.distances.{ClusterizableDistance, Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.util.SumVectors
import org.clustering4ever.scala.clusteranalysis.ClustersAnalysisCommons
/**
 *
 */
abstract class ClustersAnalysis[
    ID: Numeric,
    O,
    V: ClassTag,
    Cz <: Clusterizable[ID, O, V, Cz],
    D <: Distance[V]
](clusterized: RDD[Cz], metric: D)(implicit ct: ClassTag[Cz]) extends ClustersAnalysisCommons[ID, O, V, Cz] {

    private val neutralElement = mutable.ArrayBuffer.empty[Cz]
    def addToBuffer(buff: mutable.ArrayBuffer[Cz], elem: Cz) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[Cz], buff2: mutable.ArrayBuffer[Cz]) = buff1 ++= buff2

    lazy val datasetSize = clusterized.count

    lazy val groupedByClusterID = clusterized.map( cz => (cz.clusterID.get, cz) ).aggregateByKey(neutralElement)(addToBuffer, aggregateBuff)

    lazy val cardinalities: Map[Int, Int] = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, aggregate.size) }.collectAsMap

    lazy val clustersProportions: Map[Int, Double] = cardinalities.map{ case (clusterID, cardinality) => (clusterID, cardinality.toDouble / datasetSize) }

    lazy val centroids: Map[Int, V] = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.vector), metric)) }.collectAsMap

}
/**
 *
 */
class RealClustersAnalysis[
    ID: Numeric,
    O,
    V[Double] <: Seq[Double],
    Cz[ID, O, V <: Seq[Double]] <: Clusterizable[ID, O, V, Cz[ID, O, V]],
    D <: ContinuousDistance[V[Double]]
](clusterized: RDD[Cz[ID, O, V[Double]]], metric: D)(implicit ct: ClassTag[Cz[ID, O, V[Double]]], ct2: ClassTag[V[Double]]) extends ClustersAnalysis[ID, O, V[Double], Cz[ID, O, V[Double]], D](clusterized, metric) {

    /**
     * TO DO
     * - distributions of each features
     */

}
/**
 *
 */
class BinaryClustersAnalysis[
    ID: Numeric,
    O,
    V[Int] <: Seq[Int],
    Cz[ID, O, V <: Seq[Int]] <: Clusterizable[ID, O, V, Cz[ID, O, V]],
    D <: BinaryDistance[V[Int]]
](clusterized: RDD[Cz[ID, O, V[Int]]], metric: D, vectorHeader: Option[mutable.ArrayBuffer[String]] = None, eachCategoryRange: Option[mutable.ArrayBuffer[Int]] = None)(implicit ct: ClassTag[Cz[ID, O, V[Int]]], ct2: ClassTag[V[Int]]) extends ClustersAnalysis[ID, O, V[Int], Cz[ID, O, V[Int]], D](clusterized, metric) {

    import org.clustering4ever.util.VectorsBasicOperationsImplicits._

    if( vectorHeader.isDefined ) require(clusterized.first.vector.size == vectorHeader.size)

    lazy val occurencesPerFeature: V[Int] = clusterized.map(_.vector).reduce(SumVectors.sumVectors(_, _))

    lazy val frequencyPerFeature: Seq[Double] = occurencesPerFeature.map(_.toDouble / datasetSize)

    lazy val occurencesPerFeaturePerCluster = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, aggregate.map(_.vector).reduce(SumVectors.sumVectors(_, _))) }.collectAsMap

    lazy val frequencyPerFeaturePerCluster = occurencesPerFeaturePerCluster.map{ case (clusterID, occurences) => (clusterID, occurences.map(_.toDouble / cardinalities(clusterID))) }
}