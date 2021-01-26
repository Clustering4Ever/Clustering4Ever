package org.clustering4ever.sparkcoreextension

/**
 * @author Beck Gaël
 */
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.Row
import org.clustering4ever.clusteringtraits.{ClusteringAlgorithm, ClusteringModel, ClusteringSharedTypes}
import org.clustering4ever.roottraits.ClusteringInformationTypes._
import org.clustering4ever.roottraits.{Clusterizable, EasyClusterizable, GVector}

import scala.collection.immutable
import scala.language.higherKinds
import scala.reflect.ClassTag
/**
 * The basic trait shared by all distributed clustering algorithms
 */
trait ClusteringAlgorithmDistributed[V <: GVector[V], CA <: ClusteringModelDistributed[V]] extends ClusteringAlgorithm {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def fit[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): CA

}
/**
 *
 */
trait ClusteringModelDistributed[V <: GVector[V]] extends ClusteringModel {
	/**
	 * General methods to obtain a clustering from the model in order to measure performances scores
	 */
	protected[clustering4ever] def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): RDD[Cz[O, V]]
	/**
	 * @return a RDD of clusterIDs 
	 */
	protected[clustering4ever] final def obtainClusteringIDs[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): RDD[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last)
	}
}
/**
 *
 */
final case class ClusteringInformationsDistributed[O, V <: GVector[V], Vecto[A, B <: GVector[B]] <: VectorizationDistributed[A, B, Vecto]](
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
	final def fromRawToEasyClusterizable[GV <: GVector[GV]](row: Row, id: Int, vectorIDInRow: Int) = EasyClusterizable(row.getAs[Long](id), row, row.getAs[GV](vectorIDInRow))
	/**
	 *
	 */
	final def fromCzToRaw[O, V <: GVector[V], Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](cz: Cz[O, V]) = Row(cz)
}