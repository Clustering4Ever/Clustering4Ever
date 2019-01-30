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
import org.clustering4ever.vectorizations.{Vectorization, VectorizationLocal, EasyVectorizationLocal}
import org.clustering4ever.extensibleAlgorithmNature._
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices._
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
 * The basic trait shared by all clustering algorithms
 */
trait ClusteringAlgorithmAncestor extends ClusteringSharedTypes
/**
 * Statistic obtained during clustering algorithm executions
 */
trait ClusteringStats extends ClusteringSharedTypes
/**
 * The basic trait shared by all clustering algorithm arguments
 */
trait ClusteringModelAncestor extends ClusteringSharedTypes
/**
 * Neccessary clustering algorithm arguments to launch it 
 */
trait ClusteringModel[V <: GVector[V]] extends ClusteringModelAncestor
/**
 * The basic trait shared by all clustering algorithms working on Clusterizable
 */
trait ClusteringAlgorithm[V <: GVector[V], CM <: ClusteringModel[V]] extends ClusteringAlgorithmAncestor
/**
 * The basic trait shared by all local clustering algorithms
 */
trait ClusteringAlgorithmLocal[V <: GVector[V], CM <: ClusteringModelLocal[V]] extends ClusteringAlgorithm[V, CM] {
	/**
	 * Execute the algorithm on the given dataset
	 * @return a model of the clustering
	 */
	def run[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]]): CM
}
/**
 *
 */
trait ClusteringModelLocal[V <: GVector[V]] extends ClusteringModel[V] {
	/**
	 * General methods to obtain a clustering from the model in order to measure performances scores
	 */
	def obtainClustering[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]]): GS[Cz[ID, O, V]]
	/**
	 * Obtain clusterIDs corresponding to the given dataset
	 */
	def obtainClusteringIDs[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]]): GS[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last).asInstanceOf[GS[ClusterID]]
	}
}
/**
 * Generic concept of data which is a Collection (distributed or not) of Clusterizable
 */
trait DataExplorator[ID, O,	V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_]] extends CollectionNature[Collection] {
	/**
	 * The local or distributed collection of clusterizable
	 */
	val data: Collection[Cz[ID, O, V]]
	/**
	 * The HMap of vectorizations given by users
	 */
	val vectorizations: HMap[VectorizationMapping]
}
/**
 *
 */
// trait ClusteringInformations[O, V <: GVector[V], Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto[A, B]]] extends ClusteringSharedTypes {
// 	val clusteringInformations: immutable.HashSet[
// 		(
// 			ClusteringRunNumber,
// 			Vecto[O, V],
// 			ClusteringModel[V]
// 		)
// 	]
// }
/**
 *
 */
case class ClusteringInformationsLocal[O, V <: GVector[V], Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto]](
	val clusteringInformations: immutable.HashSet[(ClusteringRunNumber, Vecto[O, V], ClusteringModelLocal[V])] =
		immutable.HashSet.empty[(ClusteringRunNumber, Vecto[O, V], ClusteringModelLocal[V])]
) extends ClusteringSharedTypes
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
	ID,
	O,
	V <: GVector[V],
	Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Vecto <: Vectorization[O, V, Vecto],
	Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] {
	/**
	 *
	 */
	implicit val ct: ClassTag[Cz[ID, O, V]]
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
    // def updateAnyVector[GV <: GVector[GV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, GV])(implicit ct: ClassTag[Cz[ID, O, GV]]): Self[GV]
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
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Collection[_]
] extends DataExplorator[ID, O, ScalarVector[V], Cz, Collection] {

	// def featuresDistributions: Any
    
}
/**
 *
 */
trait BinaryDataExplorator[
    ID,
    O,
    V <: Seq[Int],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Collection[_]
] extends DataExplorator[ID, O, BinaryVector[V], Cz, Collection] {

	// def featuresOccurences: Any
    
}
/**
 *
 * Are undeclared val, which are defined as lazy in descendant class are really lazy ?
 */
trait ClustersAnalysis[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] {
	/**
	 *
	 */
    // val clusteringInfo: ClusteringInformations[ID, O, V, Cz, Collection]
    /**
     *
     */
	// type DistanceRestriction <: Distance[V]
    /**
     *
     */
    val datasetSize: Long
    /**
     *
     */
    // def groupedByClusterID(clusteringNumber: Int)(implicit ct: ClassTag[Cz[ID, O, V]]): Collection[(Int, mutable.ArrayBuffer[Cz[ID,O,V]])]
    /**
     *
     */
    def cardinalities(clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, Long]
    /**
     *
     */
    val cardinalitiesByClusteringNumber = mutable.HashMap.empty[ClusteringNumber, immutable.Map[ClusterID, Long]]
    /**
     *
     */
    def clustersProportions(clusteringNumber: ClusteringNumber): immutable.Map[Int, Double]
    /**
     *
     */
    val clustersProportionsByClusteringNumber = mutable.HashMap.empty[ClusteringNumber, immutable.Map[ClusterID, Double]]
    /**
     *
     */
    def centroids[D <: Distance[V]](metric: D, clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, V]
    /**
     *
     */
    val centroidsByClusteringNumber = mutable.HashMap.empty[(ClusteringNumber, MetricID), immutable.Map[Int, V]]

}
