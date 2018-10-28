package clustering4ever.scala.clusteranalysis
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{Map, mutable, GenSeq}
import clustering4ever.scala.clusterizables.{ClusterizableExt, RealClusterizable, BinaryClusterizable}
import clustering4ever.math.distances.{ClusterizableDistance, Distance, ContinuousDistance, BinaryDistance}
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.binary.Hamming
import clustering4ever.util.ClusterBasicOperations
import clustering4ever.scala.indexes.{ExternalIndexes, InternalIndexes}
/**
 *
 */
abstract class ClustersAnalysis[
    ID: Numeric,
    O,
    V,
    Cz <: ClusterizableExt[ID, V, Cz]
](clusterized: GenSeq[Cz]) {

    lazy val groupByClusterID = clusterized.groupBy(_.clusterID)

    lazy val cardinalities: Map[Int, Int] = groupByClusterID.map{ case (clusterIDOption, aggregate) => (clusterIDOption.get, aggregate.size) }.seq

    def obtainCentroids: Map[Int, V]

}
/**
 * Be cautious with other distance than Euclidean, looking for minimized medoid as centroid has a quadratic complexity  
 */
class RealClustersAnalysis[
    ID: Numeric,
    O,
    V[Double] <: Seq[Double],
    Cz[ID, O, V <: Seq[Double]] <: RealClusterizable[ID, O, V, Cz[ID, O, V]],
    D <: ContinuousDistance[V]
](clusterized: GenSeq[Cz[ID, O, V[Double]]], metric: D) extends ClustersAnalysis[ID, O, V[Double], Cz[ID, O, V[Double]]](clusterized) {

    def obtainCentroids: Map[Int, V[Double]] = groupByClusterID.map{ case (clusterIDOption, aggregate) =>
        (
            clusterIDOption.get,
            if( metric.isInstanceOf[Euclidean[V]] ) ClusterBasicOperations.obtainMean(aggregate.map(_.vector)) else ClusterBasicOperations.obtainMedoid(aggregate.map(_.vector), metric)
        )
    }.seq
}
/**
 * Be cautious with other distance than Hamming, looking for minimized medoid as centroid has a quadratic complexity
 */
class BinaryClustersAnalysis[
    ID: Numeric,
    O,
    V[Int] <: Seq[Int],
    Cz[ID, O, V <: Seq[Int]] <: BinaryClusterizable[ID, O, V, Cz[ID, O, V]],
    D <: BinaryDistance[V]
](clusterized: GenSeq[Cz[ID, O, V[Int]]], metric: D) extends ClustersAnalysis[ID, O, V[Int], Cz[ID, O, V[Int]]](clusterized) {

    def obtainCentroids: Map[Int, V[Int]] = groupByClusterID.map{ case (clusterIDOption, aggregate) =>
        (
            clusterIDOption.get,
            if( metric.isInstanceOf[Hamming[V]] ) ClusterBasicOperations.obtainMode(aggregate.map(_.vector)) else ClusterBasicOperations.obtainMedoid[Int, V, D](aggregate.map(_.vector), metric)
        )
    }.seq
}