package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */
import shapeless.ops.hlist.Mapper
import shapeless.{HList, Poly}
/**
 *
 */
final class VMapping[K, V] extends Serializable
/**
 *
 */
object VMapping extends Serializable {
	final def apply[K, V] = new VMapping[K, V]
}
/**
 *
 */
final class VectorizationMapping[K, V] extends Serializable
/**
 *
 */
object VectorizationMapping extends Serializable {
	final def apply[K, V] = new VectorizationMapping[K, V]
}
/**
 *
 */
final class ClusteringInformationsMapping[K, +V] extends Serializable
/**
 *
 */
object ClusteringInformationsMapping extends Serializable {
	final def apply[K, V] = new ClusteringInformationsMapping[K, V]
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
	final def apply[K, V] = new ModelsMapping[K, V]
}
/**
 *
 */
final class InformationsMapping[K, V] extends Serializable
/**
 *
 */
object InformationsMapping extends Serializable {
	final def apply[K, V] = new InformationsMapping[K, V]
}
/**
 *
 */
object HListRelated extends Serializable {

	final def mapOverHList[HL <: HList, R <: HList](hl: HL, p: Poly)(implicit mapper: Mapper.Aux[p.type, HL, R]): R = hl map p
}