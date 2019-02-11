package org.clustering4ever.clustering.keeper
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import shapeless.HMap
import org.clustering4ever.vectors.GVector
import org.clustering4ever.shapeless.InformationsMapping
import org.clustering4ever.clustering.{ModelsInformationsPerVectorizationAncestor, ClusteringModel, ClusteringModelLocalScalar}
import org.clustering4ever.vectorizations.VectorizationAncestor
import org.clustering4ever.types.VectorizationIDTypes._
/**
 *
 */
final case class ClusteringInformationsKeeperHMap(var infos: HMap[InformationsMapping] = HMap.empty[InformationsMapping]) extends Serializable {
	/**
	 *
	 */
	def addInformations[MI <: ModelsInformationsPerVectorizationAncestor](id: VectorizationID, info: MI): Unit = {
		implicit val mapping = InformationsMapping[VectorizationID, MI]
		infos = infos + ((id, info))
	}
	/**
	 *
	 */
	def getInformations[MI <: ModelsInformationsPerVectorizationAncestor](id: VectorizationID, mapping: InformationsMapping[Int, MI]): Option[MI] = infos.get(id)(mapping)

}
