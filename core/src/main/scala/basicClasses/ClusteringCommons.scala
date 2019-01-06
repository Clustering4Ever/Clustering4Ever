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
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping}
import org.clustering4ever.vectorizables.Vectorizable
import org.clustering4ever.math.distances.{Distance, CDB}
import org.clustering4ever.enums.ClusteringAlgorithmEnum
import org.clustering4ever.enums.ClusteringAlgorithmEnum._
/**
 * Commons properties of all clustering linked class
 */
trait ClusteringCommons extends Serializable {
	type ClusterID = Int
}
/**
 * The basic trait shared by all clustering models
 */
trait ClusteringModel extends ClusteringCommons {
	// val clusteringStats: ClusteringStats
}
/**
 * The basic trait shared by all clustering models
 */
trait ClusteringModelCz[V <: GVector[V], Collection[_]] extends ClusteringModel {
	/**
	 * General methods to obtain a clustering from the model in order to measure performances scores
	 */
	def obtainClustering[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: Collection[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): Collection[Cz[ID, O, V]]
}
/**
 * The basic trait shared by all clustering algorithms
 */
trait ClusteringAlgorithm extends ClusteringCommons
/**
 * The basic trait shared by all clustering algorithms working on Clusterizable
 */
trait ClusteringAlgorithmCz[V <: GVector[V], Collection[_], +CA <: ClusteringArgs, +CM <: ClusteringModelCz[V, Collection]] extends ClusteringAlgorithm {
	/**
	 *
	 */
	val args: CA
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: Collection[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): CM
	/**
	 *
	 */
	// def convertTowardNewVectorization[NV <: GVector[NV]]: ClusteringAlgorithmCz[NV, Collection, CA, CM]

}
/**
 * The basic trait shared by all local clustering algorithms
 */
trait LocalClusteringAlgorithm[V <: GVector[V], GS[X] <: GenSeq[X], +CA <: ClusteringArgs, +CM <: ClusteringModelCz[V, GS]] extends ClusteringAlgorithmCz[V, GS, CA, CM]
/**
 * Statistic obtained during clustering algorithm executions
 */
trait ClusteringStats extends ClusteringCommons
/**
 * Neccessary clustering algorithm arguments to launch it 
 */
trait ClusteringArgs extends Serializable {
	val algorithm: ClusteringAlgorithmEnum
}
/**
 *
 */
object NoClusteringArgs extends ClusteringArgs {
	val algorithm = AnAlgoWithoutArgs
}
/**
 * Generic concept of data which is a Collection (distributed or not) of Clusterizable
 */
trait DataExplorator[
	ID,
	O,
	V <: GVector[V],
	Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
	Collection[_]
] {
	/**
	 *
	 */
	val data: Collection[Cz[ID, O, V]]
}
/**
 * The basic trait shared by all clustering models
 */
trait Preprocessing[Collection[_]] extends Serializable {
	/**
	 * General methods to obtain a clustering from the model in order to measure performances scores
	 */
	// def preprocess[ID, V <: Seq[_], Sz[X, Y <: Seq[_]] <: DFCLG[X, Y]](data: Collection[Sz[ID, V]])(implicit ct: ClassTag[Sz[ID, V]]): Collection[Sz[ID, V]]
}
/**
 *
 */
sealed trait EmployedVectorization extends Serializable
/**
 *
 */
class DefaultWorkingVector extends EmployedVectorization
/**
 *
 */
class IthVectorization[O, V <: GVector[V]](val vectorizationID: Int, val vectorization: O => V) extends EmployedVectorization {

}
/**
 *
 */
trait ClusteringChaining[
	ID,
	O,
	V <: GVector[V],
	Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
	Collection[_],
	AlgRestrictions <: ClusteringAlgorithmCz[V, Collection, ClusteringArgs, ClusteringModelCz[V, Collection]],
	Same <: ClusteringChaining[ID, O, V, Cz, Collection, _, _, _],
	Self <: ClusteringChaining[ID, O, _, Cz, Collection, _, _, Self]
] extends DataExplorator[ID, O, V, Cz, Collection] {
	/**
	 * A securty value in order to allow proper reduce of Chaining models
	 */
	val securityReduce: Int = -1
	/**
	 *
	 */
	type ClusteringRunNumberOnASeries = Int
	/**
	 *
	 */
	type GlobalClusteringRunNumber = Int
	/**
	 *
	 */
    def fusionChainable(anotherClusterChaining: Same): Same
	/**
	 *
	 */
	val currentVectorization: EmployedVectorization
	/**
	 *
	 */
	val vectorizations: HMap[VectorizationMapping]
	/**
	 *
	 */
	val clusteringInfo: immutable.Vector[((GlobalClusteringRunNumber, ClusteringRunNumberOnASeries), EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, Collection])]
	/**
	 *
	 */
	val globalClusteringRunNumber: GlobalClusteringRunNumber
	/**
	 *
	 */
	def runAlgorithm(algorithm: AlgRestrictions, specificRunID: Int = -1)(implicit ct: ClassTag[Cz[ID, O, V]]): Self
    /**
     *
     */
    def runAlgorithms(algorithms: AlgRestrictions*)(implicit ct: ClassTag[Cz[ID, O, V]]): Self
   	/**
	 *
	 */
	def newVectorization[NV <: GVector[NV]](vectorizationID: Int, towardNewVector: O => NV)(implicit vMapping: VectorizationMapping[Int, IthVectorization[O, NV]] = new VectorizationMapping[Int, IthVectorization[O, NV]]): Self
	/**
	 *
	 */
    def updtV[NV <: GVector[NV]](vectorizationID: Int, vMapping: VMapping[Int, NV] = new VMapping[Int, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): Self
    /**
     *
     */
    def runAlgorithmsOnVectorizationEqualToDefaultVectorNature(vectorizationIDs: Seq[Int], algorithms: AlgRestrictions*)(implicit ct: ClassTag[Cz[ID, O, V]]): Self

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
    D <: Distance[V],
    Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] {

    val datasetSize: Long

    def cardinalities(clusteringNumber: Int): Map[Int, Long]

    val cardinalitiesByClusteringNumber = mutable.HashMap.empty[Int, Map[Int, Long]]

    def clustersProportions(clusteringNumber: Int): Map[Int, Double]

    val clustersProportionsByClusteringNumber = mutable.HashMap.empty[Int, Map[Int, Double]]

    val metric: D    

    def centroids(clusteringNumber: Int): Map[Int, V]

    val centroidsByClusteringNumber = mutable.HashMap.empty[Int, Map[Int, V]]

}
/**
 *
 */
trait ClustersIndexesAnalysis[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] with ClusteringCommons {
	import org.clustering4ever.enums.InternalsIndexes._
	import org.clustering4ever.enums.ExternalsIndexes._
	/**
	 *
	 */
	val internalsIndexesByClusteringNumber = mutable.HashMap.empty[Int, Map[InternalsIndexesType, Double]]
	/**
	 *
	 */
	val externalsIndexesByClusteringNumber = mutable.HashMap.empty[Int, Map[ExternalsIndexesType, Double]]
    /**
     *
     */
    def computeInternalsIndexesForEveryClusteringNumber[D <: Distance[V]](metric: D, indexes: InternalsIndexesType*): Seq[Map[InternalsIndexesType, Double]]
    /**
     * Compute given internals indexes and add result to internalsIndexesByClusteringNumber
     * @return A Map which link internal indexes to its associate value 
     */
    def computeInternalsIndexes[D <: Distance[V]](metric: D, indexes: InternalsIndexesType*)(clusteringNumber: Int = 0): Map[InternalsIndexesType, Double]
    /**
     * Compute given externals indexes and add result to externalsIndexesByClusteringNumber
     * @return A Map which link external indexes to its associate value 
     */
    def computeExternalsIndexes(groundTruth: Collection[ClusterID], indexes: ExternalsIndexesType*)(clusteringNumber: Int = 0): Map[ExternalsIndexesType, Double]
    /**
     *
     */
    def computeExternalsIndexesForEveryClusteringNumber(groundTruth: Collection[ClusterID], indexes: ExternalsIndexesType*): Seq[Map[ExternalsIndexesType, Double]]

}