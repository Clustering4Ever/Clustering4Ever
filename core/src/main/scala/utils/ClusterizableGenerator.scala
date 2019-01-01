package org.clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{mutable, immutable}
import org.clustering4ever.scala.vectorizables.Vectorizable
import org.clustering4ever.scala.clusterizables.EasyClusterizable
import org.clustering4ever.clustering.ClusteringArgs
import org.clustering4ever.scala.vectors.GVector
/**
 *
 */
object ClusterizableGenerator {
	/**
	 *
	 */
	def obtainEasyClusterizable[ID, V <: GVector](id: ID, vector: V): EasyClusterizable[ID, V, V] = new EasyClusterizable(id, new Vectorizable(vector), vector)
}