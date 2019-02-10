package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, mutable, immutable, Map}
import shapeless.HMap
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector}
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
	type ClusterID = Int
}
/**
 *
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
	 * @tparam O the raw object from where vectorizations algorithm are obtained
	 * @tparam Cz a clusterizable descendant, EasyClusterizable is the basic advise instance
	 * @tparam GS the nature of the collection which descent from GenSeq
	 * @return CM the  Clustering Model resulting from this clustering
	 */
	def run[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): CM
	/**
	 * A helper function to cast a specific model knowing from which algorithm it came from.
	 * It is usefull only for clustering chaining when models are kept into clusteringInformation HMap under their genric form ClusteringModelLocal[V]
	 * @param model: a model corresponding to this algorithm run method output, if not the cast will fail
	 */
	def castModel(model: ClusteringModelLocal[V]): CM = model.asInstanceOf[CM]
}
/**
 *
 */
trait ClusteringAlgorithmLocalScalar[V <: Seq[Double], CM <: ClusteringModelLocalScalar[V]] extends ClusteringAlgorithmLocal[ScalarVector[V], CM]
/**
 *
 */
trait ClusteringAlgorithmLocalBinary[V <: Seq[Int], CM <: ClusteringModelLocalBinary[V]] extends ClusteringAlgorithmLocal[BinaryVector[V], CM]
/**
 *
 */
trait ClusteringModelLocal[V <: GVector[V]] extends ClusteringModel {
	/**
	 * General methods to obtain a clustering on input dataset from the given model in order to measure performances scores
	 */
	protected[clustering] def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]]
	/**
	 * Obtain clusterIDs corresponding to the given dataset
	 */
	protected[clustering] def obtainClusteringIDs[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last).asInstanceOf[GS[ClusterID]]
	}
}
/**
 *
 */
trait ClusteringModelLocalScalar[V <: Seq[Double]] extends ClusteringModelLocal[ScalarVector[V]]
/**
 *
 */
trait ClusteringModelLocalBinary[V <: Seq[Int]] extends ClusteringModelLocal[BinaryVector[V]]
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
// trait SpecificClusteringInformationsLocalNew[CM <: ClusteringModelLocal] extends ClusteringInformationsGenericNew[V, CM]
/**
 *
 */
// case class ConcreteSpecificClusteringInformationsLocalNew[CM <: ClusteringModelLocal](
// 	val clusteringInformations: immutable.HashSet[(ClusteringRunNumber, CM)] = immutable.HashSet.empty[(ClusteringRunNumber, CM)]
// ) extends SpecificClusteringInformationsLocalNew[V, CM]
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
	def updateModelsInformationsPerVectorization(cnWithModels: (ClusteringRunNumber, CM)*): Unit = modelsInformationsPerVectorization ++= cnWithModels
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
case class ClusteringInformationsLocal[O, V <: GVector[V], Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto]](
	val clusteringInformations: immutable.HashSet[(ClusteringRunNumber, Vecto[O, V], ClusteringModelLocal[V])] =
		immutable.HashSet.empty[(ClusteringRunNumber, Vecto[O, V], ClusteringModelLocal[V])]
) extends ClusteringInformationsLocalGen[O, V, Vecto[O, V], ClusteringModelLocal[V]]
/**
 *
 */
case class ClusteringIndicesLocal(
	val internalsIndicesByClusteringNumberMetricVectorizationIDIndex: immutable.HashMap[(ClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double] =
		immutable.HashMap.empty[(ClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double],
	val externalsIndicesByClusteringNumberVectorizationIDIndex: immutable.HashMap[(ClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double] =
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
	 * Internal methods to merge models when runAlgorithms is launched
	 */
    // protected def fusionChainable(anotherClusterChaining: Self[V, Vecto]): Self[V, Vecto]
    /**
     * HMap containing initial and added vectorization
     * Vectorizations are accessible using corresponding vectorizationID and VectorizationMapping explicitly or implicitly (Int -> Desired-GVector)
     */
	val vectorizations: HMap[VectorizationMapping]
	/**
	 *
	 */
	// val clusteringInformations: ClusteringInformations[ID, O, Cz, Collection]
	/**
	 * Total number of algorithms launched, first run is equivalent to 0
	 */
	val clusteringRunNumber: ClusteringRunNumber = -1
	/**
	 * Run one algorithm on the current vectorization
	 */
	// def runAlgorithm(algorithm: AlgorithmsRestrictions[V]): Self[V, Vecto]
    /**
	 * Run multiples algorithms on the current vectorization
     */
    // def runAlgorithms(algorithms: AlgorithmsRestrictions[V]*): Self[V, Vecto]
   	/**
	 * @return new ClusteringChaining object with a vector of type GV and the VectorizationMapping to extract the specific vectorization
	 */
    // def addAnyVectorization[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, GV]): Self[V]
	/**
	 * @return new ClusteringChaining object with a vector of type same nature than current one and the VectorizationMapping to extract the specific vectorization
	 */
	// def addVectorization[Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto): Self[V] = {
	// 	addAnyVectorization(vectorization)
	// }
	/**
	 * Actualize the data set with a new vector of any nature
	 */
    // def updateAnyVector[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, GV])(implicit ct: ClassTag[Cz[O, GV]]): Self[GV]
	/**
	 * Actualize the data set with a new vector of the same nature than previous one
	 */
    // def updateVector[Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto): Self[V] = {
    // 	updateAnyVector[V, Vecto](vectorization)
    // }
    /**
     *
     */
    // def runAlgorithmsOnMultipleVectorizations[Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](
    //     vectorizations: Seq[Vecto],
    //     algorithms: AlgorithmsRestrictions[V]*
    // ): Self[V]
}
/**
 *
 */
trait ScalarDataExplorator[
    ID,
    O,
    V <: Seq[Double],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Collection[_]
] extends DataExplorator[O, ScalarVector[V], Cz, Collection] {

	// def featuresDistributions: Any
    
}
/**
 *
 */
trait BinaryDataExplorator[
    ID,
    O,
    V <: Seq[Int],
    Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz],
    Collection[_]
] extends DataExplorator[O, BinaryVector[V], Cz, Collection] {

	// def featuresOccurences: Any
    
}