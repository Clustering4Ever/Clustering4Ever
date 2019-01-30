package org.clustering4ever.clustering.rdd
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.immutable
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.Row
import org.apache.spark.sql.Encoders
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.shapeless.VectorizationMapping
import org.clustering4ever.vectors.GVector
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.vectorizations.{Vectorization, VectorizationDistributed}
import org.apache.spark.sql.Dataset
import org.clustering4ever.clustering.{ClusteringAlgorithm, ClusteringModel, ClusteringSharedTypes}
/**
 * The basic trait shared by all distributed clustering algorithms
 */
trait ClusteringAlgorithmDistributed[V <: GVector[V], CA <: ClusteringModelDistributed[V]] extends ClusteringAlgorithm[V, CA] {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): CA

}
/**
 *
 */
trait ClusteringModelDistributed[V <: GVector[V]] extends ClusteringModel[V] {

	// type Self <: ClusteringModelDistributed[V]

	// type AssociateAlgorithm <: ClusteringAlgorithmDistributed[V, Self]

	// def obtainAlgorithm: AssociateAlgorithm
	/**
	 * General methods to obtain a clustering from the model in order to measure performances scores
	 */
	def obtainClustering[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): RDD[Cz[ID, O, V]]
	/**
	 * @return a RDD of clusterIDs 
	 */
	def obtainClusteringIDs[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): RDD[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last)
	}
}
/**
 *
 */
case class ClusteringInformationsDistributed[O, V <: GVector[V], Vecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, Vecto]](
	val clusteringInformations: immutable.HashSet[
		(
			ClusteringRunNumber,
			Vecto[O, V],
			ClusteringModelDistributed[V]
		)
	] = immutable.HashSet.empty[(ClusteringRunNumber, Vecto[O, V], ClusteringModelDistributed[V])]
) extends ClusteringSharedTypes
/**
 *
 */
object RowToCz extends Serializable {
	/**
	 *
	 */
	def fromRawToEasyClusterizable[ID, GV <: GVector[GV]](row: Row, id: Int, vectorIDInRow: Int) = EasyClusterizable(row.getAs[ID](id), row, row.getAs[GV](vectorIDInRow))
	/**
	 *
	 */
	def fromCzToRaw[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](cz: Cz[ID, O, V]) = Row(cz)
}