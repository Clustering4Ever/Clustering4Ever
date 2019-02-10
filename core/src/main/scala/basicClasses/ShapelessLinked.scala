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
class VectorizationMapping[K, V] extends Serializable
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
/**
 *
 */
class ModelsMapping[K, V] extends Serializable
/**
 *
 */
object ModelsMapping extends Serializable {
	def apply[K, V] = new ModelsMapping[K, V]
}
/**
 *
 */
class InformationsMapping[K, V] extends Serializable
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