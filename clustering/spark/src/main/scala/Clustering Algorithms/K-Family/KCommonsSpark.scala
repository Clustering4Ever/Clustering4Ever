package clustering4ever.spark.clustering

import scala.math.pow
import scala.collection.{immutable, mutable, GenSeq, parallel}
import scala.util.Random
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import clustering4ever.math.distances.Distance
import clustering4ever.stats.Stats
import clustering4ever.scala.clusterizables.{ClusterizableExt, Clusterizable}
import clustering4ever.scala.clustering.KCommons
import clustering4ever.clustering.CommonRDDPredictClusteringModel

abstract class KCommonsSpark[
	ID: Numeric,
	N: Numeric,
	V <: Seq[N] : ClassTag,
	D <: Distance[V],
	Cz <: Clusterizable[ID, V]
	](
	data: RDD[Cz],
	metric: D,
	var k: Int,
	var initializedCenters: mutable.HashMap[Int, V] = mutable.HashMap.empty[Int, V],
	var persistanceLVL: StorageLevel = StorageLevel.MEMORY_ONLY
) extends KCommons[ID, V, D](metric)
{
	val vectorizedDataset: RDD[V] = data.map(_.vector).persist(persistanceLVL)
	val dim = vectorizedDataset.first.size
}

abstract class KCommonsModelSpark[
	ID: Numeric,
	V,
	D <: Distance[V],
	Cz <: ClusterizableExt[ID, V] : ClassTag
	](
	val centers: mutable.HashMap[Int, V],
	val metric: D
) extends CommonRDDPredictClusteringModel[V, D]
{
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input GenSeq with labels obtain via centerPredict method
	 **/
	def centerPredict(data: RDD[Cz])(implicit i: DummyImplicit): RDD[Cz] = data.map( rc => rc.setClusterID(centerPredict(rc.vector)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input GenSeq with labels obtain via knnPredict method
	 */
	def knnPredict(data: RDD[Cz], k: Int, trainDS: Seq[(ClusterID, V)])(implicit i: DummyImplicit): RDD[Cz] = data.map( rc => rc.setClusterID(knnPredict(rc.vector, k, trainDS)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input GenSeq with labels obtain via knnPredict method
	 */
	def knnPredict(data: RDD[Cz], k: Int, trainDS: Seq[Cz]): RDD[Cz] = knnPredict(data, k, trainDS.map( rc => (rc.clusterID, rc.vector) ))
}