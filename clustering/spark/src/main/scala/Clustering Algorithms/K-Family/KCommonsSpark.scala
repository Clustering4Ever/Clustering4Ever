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
import clustering4ever.clustering.{ClusteringAlgorithms, CommonPredictClusteringModel}
import clustering4ever.scala.clustering.KCommons

abstract class KCommonsSpark[
	ID: Numeric,
	N: Numeric,
	V <: immutable.Seq[N] : ClassTag,
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
