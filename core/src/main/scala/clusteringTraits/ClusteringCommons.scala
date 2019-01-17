package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, mutable, immutable, Map}
import shapeless.HMap
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.{GVector, NoGVector, ScalarVector, BinaryVector}
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
import org.clustering4ever.vectorizations.VectorizationNature
/**
 * Commons properties of all clustering linked class
 */
trait ClusteringCommons extends Serializable {
	type ClusterID = Int
}
/**
 *
 */
trait CollectionNature[Collection[_]] extends ClusteringCommons
/**
 * The basic trait shared by all clustering algorithms
 */
trait GenericClusteringAlgorithm extends ClusteringCommons
/**
 * Statistic obtained during clustering algorithm executions
 */
trait ClusteringStats extends ClusteringCommons
/**
 * Neccessary clustering algorithm arguments to launch it 
 */
trait ClusteringArgs[V <: GVector[V]] extends Serializable {
	/**
	 *
	 */
	val algorithm: ClusteringAlgorithmNature
}
/**
 * The basic trait shared by all clustering algorithms working on Clusterizable
 */
trait ClusteringAlgorithm[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_], +CA <: ClusteringArgs[V], +CM <: ClusteringModel[ID, O, V, Cz, Collection, CA]] extends GenericClusteringAlgorithm {
	/**
	 *
	 */
	protected implicit val ct: ClassTag[Cz[ID, O, V]]
	/**
	 *
	 */
	val args: CA
	/**
	 * Execute the corresponding clustering algorithm
	 * @return GenericClusteringModel
	 */
	def run(data: Collection[Cz[ID, O, V]]): CM

}
/**
 * The basic trait shared by all local clustering algorithms
 */
trait ClusteringAlgorithmLocal[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X], +CA <: ClusteringArgs[V], +CM <: ClusteringModel[ID, O, V, Cz, GS, CA]] extends ClusteringAlgorithm[ID, O, V, Cz, GS, CA, CM]
/**
 *
 */
trait ClusteringArgsLocal[V <: GVector[V]] extends ClusteringArgs[V] {
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringAlgorithmLocal[ID, O, V, Cz, GS, ClusteringArgsLocal[V], ClusteringModelLocal[ID, O, V, Cz, GS, ClusteringArgsLocal[V]]]

}
/**
 * Generic concept of data which is a Collection (distributed or not) of Clusterizable
 */
trait DataExplorator[ID, O,	V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_]] extends CollectionNature[Collection] {
	/**
	 *
	 */
	val data: Collection[Cz[ID, O, V]]
	/**
	 *
	 */
	// val currentVectorization: Vectorization[O, V]
	/**
	 *
	 */
	val vectorizations: HMap[VectorizationMapping]
}
/**
 *
 */
trait AlgorithmsRestrictions[ID, O,	V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_]] {
	/**
	 *
	 */
	type AlgorithmsRestrictions[NV <: GVector[NV]] <: ClusteringAlgorithm[ID, O, NV, Cz, Collection, ClusteringArgs[NV], ClusteringModel[ID, O, NV, Cz, Collection, ClusteringArgs[NV]]]
}
/**
 *
 */
// trait ClusteringInformations[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_]] extends CollectionNature[Collection] {
// 	val clusteringInformations: immutable.HashSet[(GlobalClusteringRunNumber, Vectorization[O, V], ClusteringArgs[V], ClusteringModel[ID, O, V, Cz, Collection, ClusteringArgs[V]])]
// 	val internalsIndicesByClusteringNumberMetricVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double]
// 	val externalsIndicesByClusteringNumberVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double]
// }
/**
 *
 */
case class ClusteringInformationsLocal[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Vecto[A, B <: GVector[B]] <: VectorizationLocal[A, B, Vecto], GS[X] <: GenSeq[X]](
	val clusteringInformations: immutable.HashSet[
		(
			GlobalClusteringRunNumber,
			Vecto[O, V],
			ClusteringArgsLocal[V],
			ClusteringModelLocal[ID, O, V, Cz, GS, ClusteringArgsLocal[V]]
		)
	] = immutable.HashSet.empty[(GlobalClusteringRunNumber, Vecto[O, V], ClusteringArgsLocal[V], ClusteringModelLocal[ID, O, V, Cz, GS, ClusteringArgsLocal[V]])],
	val internalsIndicesByClusteringNumberMetricVectorizationIDIndex: immutable.Map[
		(
			GlobalClusteringRunNumber,
			MetricID,
			VectorizationID,
			InternalsIndicesType
		),
		Double
	] = immutable.Map.empty[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double],
	val externalsIndicesByClusteringNumberVectorizationIDIndex: immutable.Map[
		(
			GlobalClusteringRunNumber,
			VectorizationID,
			ExternalsIndicesType
		),
		Double
	] = immutable.Map.empty[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double]
)
/**
 *
 */
trait ClusteringChaining[
	ID,
	O,
	V <: GVector[V],
	Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Vecto[A, B <: GVector[B]] <: Vectorization[A, B, Vecto],
	Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] with AlgorithmsRestrictions[ID, O, V, Cz, Collection] {
	/**
	 *
	 */
	implicit val ct: ClassTag[Cz[ID, O, V]]
	/**
	 *
	 */
	val chainableID: Int
	/**
	 *
	 */
	val currentVectorization: Vecto[O, V]
	/**
	 * A securty value in order to allow proper reduce of Chaining models
	 */
	protected val fusionChainableSecurity: Int
	/**
	 * Internal methods to merge models when runAlgorithms is launched
	 */
    // protected def fusionChainable(anotherClusterChaining: Self[V, Vecto]): Self[V, Vecto]
	/**
	 *
	 */
	// protected implicit val currentVectorizationMapping = currentVectorization.vectoMapping
	/**
	 *
	 */
	val vectorizations: HMap[VectorizationMapping]
	/**
	 *
	 */
	// val clusteringInformations: ClusteringInformations[ID, O, Cz, Collection]
	/**
	 * Total number of algorithms launched
	 */
	val globalClusteringRunNumber: GlobalClusteringRunNumber = -1
	/**
	 * Run one algorithm on the current vectorization
	 */
	// def runAlgorithm(algorithm: AlgorithmsRestrictions[V]): Self[V, Vecto]
    /**
	 * Run multiples algorithms on the current vectorization
     */
    // def runAlgorithms(algorithms: AlgorithmsRestrictions[V]*): Self[V, Vecto]
   	/**
	 * @return new ClusteringChaining object with a vector of type NV and the VectorizationMapping to extract the specific vectorization
	 */
    // def addAnyVectorization[NV <: GVector[NV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, NV]): Self[V]
	/**
	 * @return new ClusteringChaining object with a vector of type same nature than current one and the VectorizationMapping to extract the specific vectorization
	 */
	// def addVectorization[Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, V]): Self[V] = {
	// 	addAnyVectorization(vectorization)
	// }
	/**
	 * Actualize the data set with a new vector of any nature
	 */
    // def updateAnyVector[NV <: GVector[NV], Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): Self[NV]
	/**
	 * Actualize the data set with a new vector of the same nature than previous one
	 */
    // def updateVector[Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](vectorization: Vecto[O, V]): Self[V] = {
    // 	updateAnyVector[V, Vecto](vectorization)
    // }
    /**
     *
     */
    // def runAlgorithmsOnMultipleVectorizations[Vecto[A, B <: GVector[B]] <: Vectorization[A, B]](
    //     vectorizations: Seq[Vecto[O, V]],
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
    def centroids[D[X <: GVector[X]] <: Distance[X]](metric: D[V], clusteringNumber: ClusteringNumber): immutable.Map[ClusterID, V]
    /**
     *
     */
    val centroidsByClusteringNumber = mutable.HashMap.empty[(ClusteringNumber, MetricID), immutable.Map[Int, V]]

}
