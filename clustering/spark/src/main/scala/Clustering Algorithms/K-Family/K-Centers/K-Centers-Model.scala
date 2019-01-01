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
import org.clustering4ever.clustering.FusionedModelsDistributed
import org.clustering4ever.util.{SumVectors, ClusterBasicOperations}
import org.clustering4ever.clustering.ClusteringArgs
import org.clustering4ever.scala.vectors.GVector
/**metric
 *
 */
class KCentersModel[
	ID,
	O,
	V <: GVector,
	Cz[A, B, C <: GVector] <: Clusterizable[A, B, C, Cz],
	D <: Distance[V]
	](
	val centers: mutable.HashMap[Int, V],
	val metric: D,
	val kCentersArgs: Option[ClusteringArgs] = None
)(implicit ct: ClassTag[Cz[ID, O, V]]) extends FusionedModelsDistributed[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict(data: RDD[Cz[ID, O, V]])(implicit i: DummyImplicit): RDD[Cz[ID, O, V]] = data.map( rc => rc.addClusterID(centerPredict(rc.workingVector)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: RDD[Cz[ID, O, V]], k: Int, trainDS: Seq[(ClusterID, V)])(implicit i: DummyImplicit): RDD[Cz[ID, O, V]] = data.map( rc => rc.addClusterID(knnPredict(rc.workingVector, k, trainDS)) )
	/**
	 * Time complexity O(n<sub>data</sub>.n<sub>trainDS</sub>)
	 * @return the input Seq with labels obtain via knnPredict method
	 */
	def knnPredict(data: RDD[Cz[ID, O, V]], k: Int, trainDS: Seq[Cz[ID, O, V]], clusteringNumber: Int = 0): RDD[Cz[ID, O, V]] = knnPredict(data, k, trainDS.map( rc => (rc.clusterID(clusteringNumber), rc.workingVector) ))
}
