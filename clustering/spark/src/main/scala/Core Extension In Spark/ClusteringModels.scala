package org.clustering4ever.clustering.models
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clusterizables.Clusterizable
import org.apache.spark.sql.Dataset
import org.apache.spark.sql.Encoders
/**
 *
 */
trait CenterModelDistributed[V <: GVector[V], D <: Distance[V]] extends CenterModel[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredict(data: RDD[V]): RDD[(ClusterID, V)] = data.map( v => (centerPredict(v), v) )
}
/**
 *
 */
trait CenterModelDistributedCz[ID, O, V <: GVector[V], Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], D <: Distance[V]] extends CenterModelDistributed[V, D] with CenterModelCz[ID, O, V, Cz, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredictCz(data: RDD[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): RDD[Cz[ID, O, V]] = data.map( cz => cz.addClusterIDs(centerPredict(cz)) )
}
/**
 *
 */
trait CenterModelDistributedCzDS[ID, O, V <: GVector[V], Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], D <: Distance[V]] extends CenterModelDistributedCz[ID, O, V, Cz, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	def centerPredictCz(data: Dataset[Cz[ID, O, V]], kryoSerialization: Boolean = false)(implicit ct: ClassTag[Cz[ID, O, V]]): Dataset[Cz[ID, O, V]] = {
		if(kryoSerialization) data.map( cz => cz.addClusterIDs(centerPredict(cz)) )(Encoders.javaSerialization[Cz[ID, O, V]])
		else data.map( cz => cz.addClusterIDs(centerPredict(cz)) )(Encoders.kryo[Cz[ID, O, V]])
	}
}