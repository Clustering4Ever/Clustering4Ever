package clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{Map, GenMap, mutable, GenSeq}
import spire.math.{Numeric => SNumeric}
import clustering4ever.scala.clusterizables.{Clusterizable, RealClusterizable, BinaryClusterizable}
import clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.binary.Hamming
import clustering4ever.util.ClusterBasicOperations
import clustering4ever.util.SumVectors
import clustering4ever.scala.indexes.{ExternalIndexes, InternalIndexes}
/**
 *
 */
abstract class ClustersAnalysisCommons[
    @specialized(Int, Long) ID: Numeric,
    O,
    V,
    Cz <: Clusterizable[ID, O, V, Cz]
] {

    val datasetSize: Long

    val cardinalities: Map[Int, Int]

    val clustersProportions: Map[Int, Double]

    val centroids: Map[Int, V]
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
](clusterized: GenSeq[Cz], metric: D) extends ClustersAnalysisCommons[ID, O, V, Cz] {

    val datasetSize: Long = clusterized.size.toLong

    lazy val groupedByClusterID: GenMap[Int, GenSeq[Cz]] = clusterized.groupBy(_.clusterID.get)

    lazy val cardinalities: Map[Int, Int] = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, aggregate.size) }.seq

    lazy val clustersProportions: Map[Int, Double] = cardinalities.map{ case (clusterID, cardinality) => (clusterID, cardinality.toDouble / datasetSize) }

    lazy val centroids: Map[Int, V] = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, ClusterBasicOperations.obtainCenter(aggregate.map(_.vector), metric)) }.seq

}
/**
 *
 */
class RealClustersAnalysis[
    ID: Numeric,
    O,
    V[Double] <: Seq[Double],
    Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
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
    Cz[ID, O, V <: Seq[Int]] <: BinaryClusterizable[ID, O, V, Cz[ID, O, V]],
    D <: BinaryDistance[V[Int]]
](clusterized: GenSeq[Cz[ID, O, V[Int]]], metric: D, vectorHeader: Option[mutable.ArrayBuffer[String]] = None, eachCategoryRange: Option[mutable.ArrayBuffer[Int]] = None)(implicit ct: ClassTag[Cz[ID, O, V[Int]]], ct2: ClassTag[V[Int]]) extends ClustersAnalysis[ID, O, V[Int], Cz[ID, O, V[Int]], D](clusterized, metric) {

    import clustering4ever.util.VectorsBasicOperationsImplicits._

    if( vectorHeader.isDefined ) require(clusterized.head.vector.size == vectorHeader.size)

    lazy val occurencesPerFeature: V[Int] = clusterized.map(_.vector).reduce(SumVectors.sumVectors(_, _))

    lazy val frequencyPerFeature: Seq[Double] = occurencesPerFeature.map(_.toDouble / datasetSize)

    lazy val occurencesPerFeaturePerCluster = groupedByClusterID.map{ case (clusterID, aggregate) => (clusterID, aggregate.map(_.vector).reduce(SumVectors.sumVectors(_, _))) }.toMap

    lazy val frequencyPerFeaturePerCluster = occurencesPerFeaturePerCluster.map{ case (clusterID, occurences) => (clusterID, occurences.map(_.toDouble / cardinalities(clusterID))) }
}