package clustering4ever.spark.clustersanalysis
/**
 * @author Beck Gaël
 */
import scala.collection.{Map, mutable}
import scala.reflect.ClassTag
import scala.language.higherKinds
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import clustering4ever.scala.clusterizables.{ClusterizableExt, RealClusterizable, BinaryClusterizable}
import clustering4ever.math.distances.{ClusterizableDistance, Distance}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.util.ClusterBasicOperations
import clustering4ever.util.SumVectors
/**
 *
 */
abstract class ClustersAnalysis[
    @specialized(Int, Long) ID: Numeric,
    O,
    V,
    Cz <: ClusterizableExt[ID, V, Cz]
](clusterized: RDD[Cz])(implicit ct: ClassTag[Cz]) {

    private val neutralElement = mutable.ArrayBuffer.empty[Cz]
    def addToBuffer(buff: mutable.ArrayBuffer[Cz], elem: Cz) = buff += elem
    def aggregateBuff(buff1: mutable.ArrayBuffer[Cz], buff2: mutable.ArrayBuffer[Cz]) = buff1 ++= buff2

    lazy val datasetSize = clusterized.count

	lazy val groupedByClusterID = clusterized.map( cz => (cz.clusterID.get, cz) ).aggregateByKey(neutralElement)(addToBuffer, aggregateBuff)

	lazy val cardinalities: Map[Int, Int] = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, aggregate.size) }.collectAsMap

    val centroids: Map[Int, V]

}
/**
 *
 */
class RealClustersAnalysis[
    ID: Numeric,
    O,
    V[Double] <: Seq[Double],
    Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]]
](clusterized: RDD[Cz[ID, O, V[Double]]])(implicit ct: ClassTag[Cz[ID, O, V[Double]]], ct2: ClassTag[V[Double]]) extends ClustersAnalysis[ID, O, V[Double], Cz[ID, O, V[Double]]](clusterized) {

    lazy val centroids: Map[Int, V[Double]] = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainMean(aggregate.map(_.vector)) ) }.collectAsMap
}
/**
 *
 */
class BinaryClustersAnalysis[
    ID: Numeric,
    O,
    V[Int] <: Seq[Int],
    Cz[ID, O, V <: Seq[Int]] <: BinaryClusterizable[ID, O, V, Cz[ID, O, V]]    
](clusterized: RDD[Cz[ID, O, V[Int]]], vectorHeader: Option[mutable.ArrayBuffer[String]] = None, eachCategoryRange: Option[mutable.ArrayBuffer[Int]] = None)(implicit ct: ClassTag[Cz[ID, O, V[Int]]], ct2: ClassTag[V[Int]]) extends ClustersAnalysis[ID, O, V[Int], Cz[ID, O, V[Int]]](clusterized) {

    if( vectorHeader.isDefined ) require(clusterized.first.vector.size == vectorHeader.size)

    lazy val centroids: Map[Int, V[Int]] = groupedByClusterID.map{ case (clusterID, aggregate) =>
        (
            clusterID,
            ClusterBasicOperations.obtainMode(aggregate.map(_.vector))
        )
    }.collectAsMap

    lazy val occurencesPerFeature = clusterized.map(_.vector).reduce(SumVectors.sumVectors[Int, V](_, _))

    lazy val frequencyPerFeature = occurencesPerFeature.map(_.toDouble / datasetSize)

    lazy val occurencesPerFeaturePerCluster = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, aggregate.map(_.vector).reduce(SumVectors.sumVectors(_, _))) }.collectAsMap

    lazy val frequencyPerFeaturePerCluster = occurencesPerFeaturePerCluster.map{ case (clusterID, occurences) => (clusterID, occurences.map(_.toDouble / cardinalities(clusterID))) }
}