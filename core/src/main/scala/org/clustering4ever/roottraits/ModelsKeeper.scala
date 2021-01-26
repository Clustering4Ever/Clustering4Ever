package org.clustering4ever.roottraits

/**
 * @author Beck Gaël
 */
import org.clustering4ever.clusteringtraits.ClusteringModel
import org.clustering4ever.roottraits.ClusteringNumberType._
import shapeless.HMap

import scala.collection.mutable
import scala.language.higherKinds
/**
 *
 */
final case class ModelsKeeper(val clusteringNumbersAndNature: mutable.ArrayBuffer[(ClusteringNumber, ClusteringAlgorithmNature)] = mutable.ArrayBuffer.empty[(Int, ClusteringAlgorithmNature)], var models: HMap[ModelsMapping] = HMap.empty[ModelsMapping]) extends Serializable {
	/**
	 *
	 */
	final def addModel[CM <: ClusteringModel](clusteringNumber: ClusteringNumber, model: CM): Unit = {
		implicit val mapping = ModelsMapping[ClusteringNumber, CM]
		models = models + ((clusteringNumber, model))
		clusteringNumbersAndNature += ((clusteringNumber, model.algorithmID))
	}
	/**
	 *
	 */
	final def addModels[CM <: ClusteringModel](newModels: (CM, ClusteringNumber)*): Unit = {
		implicit val mapping = ModelsMapping[ClusteringNumber, CM]
		@annotation.tailrec
		def go(hm: HMap[ModelsMapping], newModels: List[(CM, ClusteringNumber)]): HMap[ModelsMapping] = {
			newModels match {
				case (model, clusteringNumber) :: t => {
					clusteringNumbersAndNature += ((clusteringNumber, model.algorithmID))
					go(hm + ((clusteringNumber, model)), t)
				}
				case Nil => hm
			}
		}
		models = go(models, newModels.toList)
	}
	/**
	 *
	 */
	final def getModel[CM <: ClusteringModel](clusteringNumber: ClusteringNumber, mapping: ModelsMapping[ClusteringNumber, CM]): Option[CM] = models.get(clusteringNumber)(mapping)
	/**
	 *
	 */
	final def getModels[CM <: ClusteringModel](mapping: ModelsMapping[ClusteringNumber, CM], clusteringNumbers: ClusteringNumber*): Seq[(ClusteringNumber, CM)] = {
		clusteringNumbers.flatMap(cn => models.get(cn)(mapping).map(((cn, _))))
	}
	/**
	 *
	 */
	final def getClusteringNumbersFromAlgorithmID(algorithmID: ClusteringAlgorithmNature): mutable.ArrayBuffer[ClusteringNumber] = {
		clusteringNumbersAndNature.collect{ case (clusteringNumber, algoNature) if algoNature == algorithmID => clusteringNumber }
	}
	/**
	 *
	 */
	final def getModelsFromAlgorithmID[CM <: ClusteringModel](algorithmID: ClusteringAlgorithmNature, mapping: ModelsMapping[ClusteringNumber, CM]): Seq[(ClusteringNumber, CM)] = {
		val clusteringNumbers = getClusteringNumbersFromAlgorithmID(algorithmID)
		getModels(mapping, clusteringNumbers:_*)
	}
}