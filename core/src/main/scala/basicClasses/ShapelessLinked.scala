package org.clustering4ever.shapeless
/**
 * @author Beck Gaël
 */
import shapeless.Poly
import shapeless.HList
import shapeless.ops.hlist.Mapper
import org.clustering4ever.vectors.GVector
/**
 *
 */
final class VMapping[K, V] extends Serializable
/**
 *
 */
object VMapping extends Serializable {
	def apply[K, V] = new VMapping[K, V]
}
/**
 *
 */
final class VectorizationMapping[K, V] extends Serializable
/**
 *
 */
object VectorizationMapping extends Serializable {
	def apply[K, V] = new VectorizationMapping[K, V]
}
/**
 *
 */
final class ClusteringInformationsMapping[K, +V] extends Serializable
/**
 *
 */
object ClusteringInformationsMapping extends Serializable {
	def apply[K, V] = new ClusteringInformationsMapping[K, V]
}
/**
 *
 */
final class DistancesMapping[K, V] extends Serializable
/**
 *
 */
final class ModelsMapping[K, V] extends Serializable
/**
 *
 */
object ModelsMapping extends Serializable {
	def apply[K, V] = new ModelsMapping[K, V]
}
/**
 *
 */
final class InformationsMapping[K, V] extends Serializable
/**
 *
 */
object InformationsMapping extends Serializable {
	def apply[K, V] = new InformationsMapping[K, V]
}
/**
 *
 */
object HListRelated extends Serializable {

	def mapOverHList[HL <: HList, R <: HList](hl: HL, p: Poly)(implicit mapper: Mapper.Aux[p.type, HL, R]): R = hl map p
}