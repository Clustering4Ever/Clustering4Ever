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
	final def centerPredict(data: RDD[V]): RDD[(ClusterID, V)] = data.map( v => (centerPredict(v), v) )
}
/**
 *
 */
trait CenterModelDistributedCz[V <: GVector[V], D <: Distance[V]] extends CenterModelDistributed[V, D] with CenterModelCz[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	final def centerPredictCz[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: RDD[Cz[O, V]])(implicit ct: ClassTag[Cz[O, V]]): RDD[Cz[O, V]] = data.map( cz => cz.addClusterIDs(centerPredict(cz)) )
}
/**
 *
 */
trait CenterModelDistributedCzDS[V <: GVector[V], D <: Distance[V]] extends CenterModelDistributedCz[V, D] {
	/**
	 * Time complexity O(n<sub>data</sub>.c) with c the number of clusters
	 * @return the input Seq with labels obtain via centerPredict method
	 */
	final def centerPredictCz[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz]](data: Dataset[Cz[O, V]], kryoSerialization: Boolean = false)(implicit ct: ClassTag[Cz[O, V]]): Dataset[Cz[O, V]] = {
		if(kryoSerialization) data.map( cz => cz.addClusterIDs(centerPredict(cz)) )(Encoders.javaSerialization[Cz[O, V]])
		else data.map( cz => cz.addClusterIDs(centerPredict(cz)) )(Encoders.kryo[Cz[O, V]])
	}
}