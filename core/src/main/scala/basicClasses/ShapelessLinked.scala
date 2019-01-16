package org.clustering4ever.shapeless
/**
 * @author Beck Gaël
 */
import org.clustering4ever.vectors.GVector
/**
 *
 */
class VMapping[K, V] extends Serializable
/**
 *
 */
object VMapping extends Serializable {
	def apply[K, V] = new VMapping[K, V]
}
/**
 *
 */
class VectorizationMapping[K, +V] extends Serializable
/**
 *
 */
object VectorizationMapping extends Serializable {
	def apply[K, V] = new VectorizationMapping[K, V]
}
/**
 *
 */
class ClusteringInformationsMapping[K, +V] extends Serializable
/**
 *
 */
object ClusteringInformationsMapping extends Serializable {
	def apply[K, V] = new ClusteringInformationsMapping[K, V]
}
/**
 *
 */
class DistancesMapping[K, V] extends Serializable
