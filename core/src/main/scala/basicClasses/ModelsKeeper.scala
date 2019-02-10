package org.clustering4ever.clustering.keeper
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.mutable
import shapeless.{HList, HNil, HMap}
import org.clustering4ever.vectors.GVector
import org.clustering4ever.shapeless.{HListRelated, ModelsMapping}
import org.clustering4ever.clustering.{ModelsInformationsPerVectorization, ClusteringModel, ClusteringModelLocalScalar}
import org.clustering4ever.vectorizations.VectorizationAncestor
import org.clustering4ever.types.ClusteringNumberType._
import  org.clustering4ever.extensibleAlgorithmNature.ClusteringAlgorithmNature
/**
 *
 */
case class ModelsKeeper(val clusteringNumbersAndNature: mutable.ArrayBuffer[(ClusteringNumber, ClusteringAlgorithmNature)] = mutable.ArrayBuffer.empty[(Int, ClusteringAlgorithmNature)], var models: HMap[ModelsMapping] = HMap.empty[ModelsMapping]) extends Serializable {
	/**
	 *
	 */
	def addModel[CM <: ClusteringModel](clusteringNumber: ClusteringNumber, model: CM): Unit = {
		implicit val mapping = ModelsMapping[ClusteringNumber, CM]
		models = models + ((clusteringNumber, model))
		clusteringNumbersAndNature += ((clusteringNumber, model.algorithmID))
	}
	/**
	 *
	 */
	def addModels[CM <: ClusteringModel](newModels: (CM, ClusteringNumber)*): Unit = {
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
	def getModel[CM <: ClusteringModel](clusteringNumber: ClusteringNumber, mapping: ModelsMapping[ClusteringNumber, CM]): Option[CM] = models.get(clusteringNumber)(mapping)
	/**
	 *
	 */
	def getModels[CM <: ClusteringModel](mapping: ModelsMapping[ClusteringNumber, CM], clusteringNumbers: ClusteringNumber*): Seq[(ClusteringNumber, CM)] = {
		clusteringNumbers.map( cn => models.get(cn)(mapping).map(((cn, _))) ).flatten
	}
	/**
	 *
	 */
	def getClusteringNumbersFromAlgorithmID(algorithmID: ClusteringAlgorithmNature): mutable.ArrayBuffer[ClusteringNumber] = {
		clusteringNumbersAndNature.collect{ case (clusteringNumber, algoNature) if algoNature == algorithmID => clusteringNumber }
	}
	/**
	 *
	 */
	def getModelsFromAlgorithmID[CM <: ClusteringModel](algorithmID: ClusteringAlgorithmNature, mapping: ModelsMapping[ClusteringNumber, CM]): Seq[(ClusteringNumber, CM)] = {
		val clusteringNumbers = getClusteringNumbersFromAlgorithmID(algorithmID)
		getModels(mapping, clusteringNumbers:_*)
	}
}