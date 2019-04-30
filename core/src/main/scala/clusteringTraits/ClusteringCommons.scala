package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import _root_.scala.language.higherKinds
import _root_.scala.reflect.ClassTag
import _root_.scala.collection.{GenSeq, mutable, immutable}
import _root_.scala.util.Try
import shapeless.HMap
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, ClusteringInformationsMapping}
import org.clustering4ever.vectorizables.Vectorizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectorizations.{VectorizationAncestor, Vectorization, VectorizationLocal, EasyVectorizationLocal}
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices._
import org.clustering4ever.extensibleAlgorithmNature.ClusteringAlgorithmNature
/**
 * Commons properties of all clustering linked class
 */
trait ClusteringSharedTypes extends Serializable {
	/**
	 * Clustering Identifier, an Int which define in which cluster is fallen a point
	 */
	final type ClusterID = Int
}
/**
 * Type of collection used, <: GenSeq, RDD, Dataset
 * @tparam Collection type of collection used, <: GenSeq, RDD, Dataset, others
 */
trait CollectionNature[Collection[_]] extends ClusteringSharedTypes
/**
 * Statistic obtained during clustering algorithm executions
 */
trait ClusteringStats extends ClusteringSharedTypes
/**
 * The basic trait shared by all clustering algorithms
 */
trait ClusteringAlgorithm extends ClusteringSharedTypes {
	/**
	 * The algorithm identifier
	 */
	val algorithmID: ClusteringAlgorithmNature
}
/**
 * The basic trait shared by all clustering algorithm models
 */
trait ClusteringModel extends ClusteringSharedTypes {
	/**
	 * The algorithm identifier
	 */
	val algorithmID: ClusteringAlgorithmNature	
}
/**
 * The basic trait shared by all local clustering algorithms
 * @tparam V the nature of the vector used in this algorithm
 * @tparam CM the corresponding Clustering Model to this algorithm
 */
trait ClusteringAlgorithmLocal[V <: GVector[V], CM <: ClusteringModelLocal[V]] extends ClusteringAlgorithm {
	/**
	 * Execute the algorithm on the given dataset
	 * @tparam O the raw object from where vectorizations are obtained
	 * @tparam Cz a clusterizable descendant, EasyClusterizable is the basic advise instance
	 * @tparam GS the nature of the collection which descent from GenSeq
	 * @return the Clustering Model resulting from this clustering
	 */
	def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): CM
	/**
	 * A helper function to cast a specific model knowing from which algorithm it came from.
	 * It is usefull for clustering chaining when models are kept into clusteringInformation HMap under their generic form ClusteringModelLocal[V]
	 * @param model: a model corresponding to this algorithm run method output, if not the cast will fail
	 */
	final def castModel(model: ClusteringModelLocal[V]): Option[CM] = Try(model.asInstanceOf[CM]).toOption
}
/**
 * @tparam V
 * @tparam CM
 */
trait ClusteringAlgorithmLocalScalar[CM <: ClusteringModelLocalScalar] extends ClusteringAlgorithmLocal[ScalarVector, CM]
/**
 * @tparam V
 * @tparam CM
 */
trait ClusteringAlgorithmLocalBinary[CM <: ClusteringModelLocalBinary] extends ClusteringAlgorithmLocal[BinaryVector, CM]
/**
 *
 * @tparam CM
 */
trait ClusteringAlgorithmLocalMixed[CM <: ClusteringModelLocalMixed] extends ClusteringAlgorithmLocal[MixedVector, CM]
/**
 * @tparam V
 */
trait ClusteringModelLocal[V <: GVector[V]] extends ClusteringModel {
	/**
	 * General methods to obtain a clustering on input dataset from the given model in order to measure performances scores
	 */
	protected[clustering] def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]]
	/**
	 * Obtain clusterIDs corresponding to the given dataset
	 */
	protected[clustering] final def obtainClusteringIDs[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last).asInstanceOf[GS[ClusterID]]
	}
}
/**
 * @tparam V
 */
trait ClusteringModelLocalScalar extends ClusteringModelLocal[ScalarVector]
/**
 * @tparam V
 */
trait ClusteringModelLocalBinary extends ClusteringModelLocal[BinaryVector]
/**
 * @tparam Vb
 * @tparam Vs
 */
trait ClusteringModelLocalMixed extends ClusteringModelLocal[MixedVector]
/**
 * Generic concept of data which is a Collection (distributed or not) of Clusterizable
 */
trait DataExplorator[O,	V <: GVector[V], Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], Collection[_]] extends CollectionNature[Collection] {
	/**
	 * The local or distributed collection of clusterizable
	 */
	val data: Collection[Cz[O, V]]
}
/**
 *
 */
trait DataExploratorWithVectorization[O, V <: GVector[V], Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], Collection[_]] extends DataExplorator[O, V, Cz, Collection] {
	/**
	 * The HMap of vectorizations given by users
	 */
	val vectorizations: HMap[VectorizationMapping]
}
/**
 *
 */
trait ClusteringInformationsGenericNew[CM <: ClusteringModel] extends ClusteringSharedTypes {
	val clusteringInformations: immutable.HashSet[(ClusteringRunNumber, CM)]
}
/**
 *
 */
trait ModelsInformationsPerVectorizationAncestor extends ClusteringSharedTypes 
/**
 *
 */
trait ModelsInformationsPerVectorization[Vecto <: VectorizationAncestor[Vecto], CM <: ClusteringModel] extends ModelsInformationsPerVectorizationAncestor {
	/**
	 *
	 */
	val vectorization: Vecto
	/**
	 *
	 */
	val modelsInformationsPerVectorization: mutable.ArrayBuffer[(ClusteringRunNumber, CM)]
	/**
	 *
	 */
	final def updateModelsInformationsPerVectorization(cnWithModels: (ClusteringRunNumber, CM)*): Unit = modelsInformationsPerVectorization ++= cnWithModels
}
/**
 *
 */
trait ClusteringInformations[Vecto <: VectorizationAncestor[Vecto], CM <: ClusteringModel] extends ClusteringSharedTypes {
	val clusteringInformations: immutable.HashSet[(ClusteringRunNumber, Vecto, CM)]
}
/**
 *
 */
trait ClusteringInformationsLocalGen[O, V <: GVector[V], Vecto <: Vectorization[O, V, Vecto], CM <: ClusteringModel] extends ClusteringInformations[Vecto, CM]
/**
 *
 */
final case class ClusteringInformationsLocal[O, V <: GVector[V], Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto]](
	final val clusteringInformations: immutable.HashSet[(ClusteringRunNumber, Vecto[O, V], ClusteringModelLocal[V])] =
		immutable.HashSet.empty[(ClusteringRunNumber, Vecto[O, V], ClusteringModelLocal[V])]
) extends ClusteringInformationsLocalGen[O, V, Vecto[O, V], ClusteringModelLocal[V]]
/**
 *
 */
final case class ClusteringIndicesLocal(
	final val internalsIndicesByClusteringNumberMetricVectorizationIDIndex: immutable.HashMap[(ClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double] =
		immutable.HashMap.empty[(ClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double],
	final val externalsIndicesByClusteringNumberVectorizationIDIndex: immutable.HashMap[(ClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double] =
		immutable.HashMap.empty[(ClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double]
) extends ClusteringSharedTypes
/**
 *
 */
trait ClusteringChaining[
	O,
	V <: GVector[V],
	Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Vecto <: Vectorization[O, V, Vecto],
	Collection[_]
] extends DataExplorator[O, V, Cz, Collection] {
	/**
	 *
	 */
	implicit val ct: ClassTag[Cz[O, V]]
	/**
	 * The ID of this clustering chainable, it stays constant over algorithms launch
	 */
	val chainableID: Int
	/**
	 * The current vectorization employed for the field v of clusterizable in the dataset
	 */
	val currentVectorization: Vecto
	/**
	 * A securty value in order to allow proper reduce of Chaining models
	 */
	protected val fusionChainableSecurity: Int
    /**
     * HMap containing initial and added vectorization
     * Vectorizations are accessible using corresponding vectorizationID and VectorizationMapping explicitly or implicitly (Int -> Desired-GVector)
     */
	val vectorizations: HMap[VectorizationMapping]
	/**
	 * Total number of algorithms launched, first run is equivalent to 0
	 */
	val clusteringRunNumber: ClusteringRunNumber = -1

}
/**
 *
 */
trait ScalarDataExplorator[
    ID,
    O,
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Collection[_]
] extends DataExplorator[O, ScalarVector, Cz, Collection] {

	// def featuresDistributions: Any
    
}
/**
 *
 */
trait BinaryDataExplorator[
    ID,
    O,
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Collection[_]
] extends DataExplorator[O, BinaryVector, Cz, Collection] {

	// def featuresOccurences: Any
    
}