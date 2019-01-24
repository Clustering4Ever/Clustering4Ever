package org.clustering4ever.clustering
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
import org.clustering4ever.shapeless.{VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.vectors.GVector
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.vectorizations.Vectorization
import org.apache.spark.sql.Dataset
/**
 * The basic trait shared by all distributed clustering algorithms
 */
trait ClusteringAlgorithmDistributed[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], +CA <: ClusteringArgsDistributed[V], +CM <: ClusteringModelDistributed[ID, O, V, Cz, CA]] extends ClusteringAlgorithm[ID, O, V, Cz, RDD, CA, CM] {
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
case class ClusteringInformationsDistributed[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Vecto <: VectorizationDistributed[O, V, Vecto]](
	val clusteringInformations: immutable.HashSet[
		(
			GlobalClusteringRunNumber,
			Vecto,
			ClusteringArgsDistributed[V],
			ClusteringModelDistributed[ID, O, V, Cz, ClusteringArgsDistributed[V]]
		)
	] = immutable.HashSet.empty[(GlobalClusteringRunNumber, Vecto, ClusteringArgsDistributed[V], ClusteringModelDistributed[ID, O, V, Cz, ClusteringArgsDistributed[V]])]
) extends ClusteringCommons
/**
 *
 */
trait VectorizationDistributed[O, V <: GVector[V], Self <: VectorizationDistributed[O, V, Self]] extends Vectorization[O, V, Self] {
	/**
	 *
	 */
	this: Self =>
	/**
	 *
	 */
	def getInformationMapping[ID, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](cz: Option[RDD[Cz[ID, O, V]]] = None): ClusteringInformationsMapping[VectorizationID, Self] = {
		ClusteringInformationsMapping[VectorizationID, Self]
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
) extends VectorizationDistributed[O, V, EasyVectorizationDistributed[O, V]] {
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
	 * @return the corresponding algorithm with given arguments to run on data
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringAlgorithmDistributed[ID, O, V, Cz, ClusteringArgsDistributed[V], ClusteringModelDistributed[ID, O, V, Cz, ClusteringArgsDistributed[V]]]
}
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