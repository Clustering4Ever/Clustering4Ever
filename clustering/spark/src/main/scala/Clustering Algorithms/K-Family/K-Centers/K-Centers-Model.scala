package org.clustering4ever.spark.clustering.kcenters

import scala.language.higherKinds
import org.apache.commons.math3.distribution.EnumeratedDistribution
import org.apache.commons.math3.util.Pair
import scala.collection.JavaConverters._
import scala.math.pow
import scala.collection.{immutable, mutable, parallel}
import scala.util.Random
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.stats.Stats
import org.clustering4ever.scala.clusterizables.Clusterizable
import org.clustering4ever.scala.clustering.kcenters.KCommons
import org.clustering4ever.clustering.CenterOrientedModelDistributed
import org.clustering4ever.util.{SumVectors, ClusterBasicOperations}
import org.clustering4ever.scala.measurableclass.BinaryScalarVector
import spire.math.{Numeric => SNumeric}
/**
 *
 */
class KCentersModel[
	ID: Numeric,
	O,
	V,
	Cz <: Clusterizable[ID, O, V, Cz] : ClassTag,
	D <: Distance[V]
	](
	val centers: mutable.HashMap[Int, V],
	val metric: D
) extends CenterOrientedModelDistributed[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict(data: RDD[Cz])(implicit i: DummyImplicit): RDD[Cz] = data.map( rc => rc.addClusterID(centerPredict(rc.vector)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: RDD[Cz], k: Int, trainDS: Seq[(ClusterID, V)])(implicit i: DummyImplicit): RDD[Cz] = data.map( rc => rc.addClusterID(knnPredict(rc.vector, k, trainDS)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: RDD[Cz], k: Int, trainDS: Seq[Cz]): RDD[Cz] = knnPredict(data, k, trainDS.map( rc => (rc.clusterID.get, rc.vector) ))
}
