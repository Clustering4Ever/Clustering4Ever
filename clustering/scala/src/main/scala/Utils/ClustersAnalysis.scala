package org.clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{Map, GenMap, mutable, GenSeq}
import spire.math.{Numeric => SNumeric}
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
import org.clustering4ever.math.distances.scalar.Euclidean
import org.clustering4ever.math.distances.binary.Hamming
import org.clustering4ever.util.ClusterBasicOperations
import org.clustering4ever.util.SumVectors
import org.clustering4ever.scala.indexes.{ExternalIndexes, InternalIndexes}
/**
 *
 * Are undeclared val, which are defined as lazy in descendant class are really lazy ?
 */
abstract class ClustersAnalysisCommons[
    @specialized(Int, Long) ID: Numeric,
    O,
    V,
    Cz <: Clusterizable[ID, O, V, Cz]
] extends Serializable {

    val datasetSize: Long

    val cardinalities: Map[Int, Int]

    val clustersProportions: Map[Int, Double]

    def centroids(workingVector: Int = 0): Map[Int, V]
}
/**
 *
 */
abstract class ClustersAnalysis[
    ID: Numeric,
    O,
    V,
    Cz <: Clusterizable[ID, O, V, Cz],
    D <: Distance[V]
](clusterized: GenSeq[Cz], metric: D, clusteringNumber: Int = 0) extends ClustersAnalysisCommons[ID, O, V, Cz] {

    val datasetSize: Long = clusterized.size.toLong

    lazy val groupedByClusterID: GenMap[Int, GenSeq[Cz]] = clusterized.groupBy(_.clusterID(clusteringNumber))

    lazy val cardinalities: Map[Int, Int] = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, aggregate.size) }.seq

    lazy val clustersProportions: Map[Int, Double] = cardinalities.map{ case (clusterID, cardinality) => (clusterID, cardinality.toDouble / datasetSize) }

    def centroids(workingVector: Int = 0): Map[Int, V] = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.vector(workingVector)), metric)) }.seq

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
](clusterized: GenSeq[Cz[ID, O, V[Double]]], metric: D)(implicit ct: ClassTag[Cz[ID, O, V[Double]]], ct2: ClassTag[V[Double]]) extends ClustersAnalysis[ID, O, V[Double], Cz[ID, O, V[Double]], D](clusterized, metric) {

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
](clusterized: GenSeq[Cz[ID, O, V[Int]]], metric: D, vectorHeader: Option[mutable.ArrayBuffer[String]] = None, eachCategoryRange: Option[mutable.ArrayBuffer[Int]] = None)(implicit ct: ClassTag[Cz[ID, O, V[Int]]], ct2: ClassTag[V[Int]]) extends ClustersAnalysis[ID, O, V[Int], Cz[ID, O, V[Int]], D](clusterized, metric) {

    private val originalVector = 0

    import org.clustering4ever.util.VectorsBasicOperationsImplicits._

    if(vectorHeader.isDefined) require(clusterized.head.vector(originalVector).size == vectorHeader.size)

    lazy val occurencesPerFeature: V[Int] = clusterized.map(_.vector(originalVector)).reduce(SumVectors.sumVectors(_, _))

    lazy val frequencyPerFeature: Seq[Double] = occurencesPerFeature.map(_.toDouble / datasetSize)

    lazy val occurencesPerFeaturePerCluster = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, aggregate.map(_.vector(originalVector)).reduce(SumVectors.sumVectors(_, _))) }.toMap

    lazy val frequencyPerFeaturePerCluster = occurencesPerFeaturePerCluster.map{ case (clusterID, occurences) => (clusterID, occurences.map(_.toDouble / cardinalities(clusterID))) }
}