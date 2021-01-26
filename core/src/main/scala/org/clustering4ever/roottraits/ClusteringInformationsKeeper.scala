package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */
import org.clustering4ever.clusteringtraits.ModelsInformationsPerVectorizationAncestor
import org.clustering4ever.roottraits.VectorizationIDTypes._
import shapeless.HMap

import scala.language.higherKinds
/**
 *
 */
final case class ClusteringInformationsKeeperHMap(var infos: HMap[InformationsMapping] = HMap.empty[InformationsMapping]) extends Serializable {
	/**
	 *
	 */
	final def addInformations[MI <: ModelsInformationsPerVectorizationAncestor](id: VectorizationID, info: MI): Unit = {
		implicit val mapping = InformationsMapping[VectorizationID, MI]
		infos = infos + ((id, info))
	}
	/**
	 *
	 */
	final def getInformations[MI <: ModelsInformationsPerVectorizationAncestor](id: VectorizationID, mapping: InformationsMapping[Int, MI]): Option[MI] = infos.get(id)(mapping)
}
