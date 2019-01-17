package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable
import org.apache.spark.rdd.RDD
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.shapeless.{VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.vectors.GVector
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.vectorizations.Vectorization
/**
 * The basic trait shared by all distributed clustering algorithms
 */
trait ClusteringAlgorithmDistributed[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], +CA <: ClusteringArgs[V], +CM <: ClusteringModel[ID, O, V, Cz, RDD, CA]] extends ClusteringAlgorithm[ID, O, V, Cz, RDD, CA, CM] {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return GenericClusteringModel
	 */
	def run(data: RDD[Cz[ID, O, V]]): CM

}
/**
 *
 */
trait ClusteringModelDistributed[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], +CA <: ClusteringArgsDistributed[V]] extends ClusteringModel[ID, O, V, Cz, RDD, CA] {
	/**
	 *
	 */
	def obtainClusteringIDs(data: RDD[Cz[ID, O, V]]): RDD[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last)
	}
}
/**
 *
 */
case class ClusteringInformationsDistributed[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Vecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, Vecto]](
	val clusteringInformations: immutable.HashSet[
		(
			GlobalClusteringRunNumber,
			Vecto[O, V],
			ClusteringArgsDistributed[V],
			ClusteringModelDistributed[ID, O, V, Cz, ClusteringArgsDistributed[V]]
		)
	] = immutable.HashSet.empty[(GlobalClusteringRunNumber, Vecto[O, V], ClusteringArgsDistributed[V], ClusteringModelDistributed[ID, O, V, Cz, ClusteringArgsDistributed[V]])],
	val internalsIndicesByClusteringNumberMetricVectorizationIDIndex: immutable.Map[
		(
			GlobalClusteringRunNumber,
			MetricID,
			VectorizationID,
			InternalsIndicesType
		),
			Double
	] = immutable.Map.empty[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double],
	val externalsIndicesByClusteringNumberVectorizationIDIndex: immutable.Map[
			(	GlobalClusteringRunNumber,
				VectorizationID,
				ExternalsIndicesType
			),
			Double
	] = immutable.Map.empty[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double]
)
/**
 *
 */
trait VectorizationDistributed[O, V <: GVector[V], Self[A, B <: GVector[B]] <: VectorizationDistributed[A, B, Self]] extends Vectorization[O, V, Self] {
	/**
	 *
	 */
	def getInformationMapping[ID, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](cz: Option[RDD[Cz[ID, O, V]]] = None): ClusteringInformationsMapping[VectorizationID, Self[O, V]] = {
		ClusteringInformationsMapping[VectorizationID, Self[O, V]]
	}
}
/**
 *
 */
case class EasyVectorizationDistributed[O, V <: GVector[V]] (
	val vectorizationID: VectorizationID,
	val vectorizationFct: Option[O => V] = None,
	val clusteringNumbers: immutable.HashSet[Int] = immutable.HashSet.empty[Int],
	val outputFeaturesNames: immutable.Vector[String] = immutable.Vector.empty[String]
) extends VectorizationDistributed[O, V, EasyVectorizationDistributed] {
	/**
	 *
	 */
	def updateClustering(clusteringIDs: Int*): EasyVectorizationDistributed[O, V] = copy(clusteringNumbers = clusteringNumbers ++ clusteringIDs)
}
/**
 *
 */
trait ClusteringArgsDistributed[V <: GVector[V]] extends ClusteringArgs[V] {
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringAlgorithmDistributed[ID, O, V, Cz, ClusteringArgsDistributed[V], ClusteringModelDistributed[ID, O, V, Cz, ClusteringArgsDistributed[V]]]
}