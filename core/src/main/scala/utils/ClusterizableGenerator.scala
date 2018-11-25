package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import org.clustering4ever.scala.vectorizables.{Vector, MixtVector}
import org.clustering4ever.scala.clusterizables.{EasyClusterizable, EasyClusterizableExt}
import org.clustering4ever.scala.measurableclass.BinaryScalarVector
/**
 *
 */
object ClusterizableGenerator {
	/**
	 *
	 */
	def obtainEasyClusterizable[@specialized(Int, Long) ID: Numeric, V](id: ID, vector: V): EasyClusterizable[ID, V, V] = new EasyClusterizable(id, new Vector(vector))
	/**
	 *
	 */
	def obtainEasyClusterizableExt[@specialized(Int, Long) ID: Numeric, N, V](id: ID, vector: V): EasyClusterizableExt[ID, V, V] = new EasyClusterizableExt[ID, V, V](id, new Vector[V](vector))
}