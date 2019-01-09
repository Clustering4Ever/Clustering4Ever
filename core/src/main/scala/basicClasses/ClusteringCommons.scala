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
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, DistancesMapping}
import org.clustering4ever.vectorizables.Vectorizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectorizations.{EmployedVectorization, DefaultWorkingVector, IthVectorization}
import org.clustering4ever.extensibleAlgorithmNature._
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
	/**
	 * Obtain only clusterIDs
	 */
	def obtainClusteringIDs[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: Collection[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): Collection[ClusterID]
}
/**
 *
 */
trait ClusteringModelLocal[V <: GVector[V], GS[X] <: GenSeq[X]] extends ClusteringModelCz[V, GS] {
	/**
	 *
	 */
	def obtainClusteringIDs[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]](data: GS[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): GS[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last).asInstanceOf[GS[ClusterID]]
	}
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
	val algorithm: ClusteringAlgorithmNature
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
trait DataExplorator[ID, O,	V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_]] {
	/**
	 *
	 */
	val data: Collection[Cz[ID, O, V]]
}
/**
 *
 */
trait ClusteringChaining[
	ID,
	O,
	V <: GVector[V],
	Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
	Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] {
	/**
	 *
	 */
	type Self[NV <: GVector[NV]] <: ClusteringChaining[ID, O, NV, Cz, Collection]
	/**
	 *
	 */
	type AlgorithmsRestrictions[NV <: GVector[NV]] <: ClusteringAlgorithmCz[NV, Collection, ClusteringArgs, ClusteringModelCz[NV, Collection]]
    /**
     *
     */
    val initialVectorNatureMapping = new VMapping[Int, V]
	/**
	 * A securty value in order to allow proper reduce of Chaining models
	 */
	protected val fusionChainableSecurity: Int = 0
	/**
	 *
	 */
	protected val runNumberWhenMultiRuns: Int = - 1
	/**
	 *
	 */
	val chainableID: Int
	/**
	 *
	 */
	type ClusteringRunNumberOnASeries = Int
	/**
	 *
	 */
	type GlobalClusteringRunNumber = Int
	/**
	 * Internal methods to merge models when runAlgorithms is launched
	 */
    protected def fusionChainable(anotherClusterChaining: Self[V]): Self[V]
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
	val globalClusteringRunNumber: GlobalClusteringRunNumber = 0
	/**
	 *
	 */
	def runAlgorithm(algorithm: AlgorithmsRestrictions[V])(implicit ct: ClassTag[Cz[ID, O, V]]): Self[V]
    /**
     *
     */
    def runAlgorithms(algorithms: AlgorithmsRestrictions[V]*)(implicit ct: ClassTag[Cz[ID, O, V]]): Self[V]
   	/**
	 *
	 */
    def addAnyVectorizationNature[NV <: GVector[NV]](vectorizationNature: Int, vectorizationID: Int, towardNewVector: O => NV): Self[V]
	/**
	 *
	 */
	def addDefaultVectorizationNature(vectorizationID: Int, towardNewVector: O => V): Self[V] = addAnyVectorizationNature(0, vectorizationID, towardNewVector)
	/**
	 *
	 */
    def updtV[NV <: GVector[NV]](vectorizationNature: Int, vectorizationID: Int, vMapping: VMapping[Int, NV] = new VMapping[Int, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): Self[NV]
	/**
	 *
	 */
    def updtVDefaultNature[NV <: GVector[NV]](vectorizationID: Int, vMapping: VMapping[Int, NV] = new VMapping[Int, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): Self[NV] = {
    	updtV(0, vectorizationID, vMapping)
    }
    /**
     *
     */
    protected def applyDifferentVectorizationsOfSameNature[NV <: GVector[NV]](
    	vectorizationNature: Int,
        vectorizationIDs: Seq[Int],
        algorithms: Seq[AlgorithmsRestrictions[NV]],
        otherVectorizationMapping: VMapping[Int, NV] = new VMapping[Int, NV]
    )(implicit ct: ClassTag[Cz[ID, O, NV]]): Self[NV]

    /**
     *
     */
    def runAlgorithmsOnVectorsWithCurrentVectorization(vectorizationIDs: Seq[Int], algorithms: AlgorithmsRestrictions[V]*)(implicit ct: ClassTag[Cz[ID, O, V]]): Self[V] = {
        applyDifferentVectorizationsOfSameNature(0, vectorizationIDs, algorithms, initialVectorNatureMapping)
    }
    /**
     *
     */
    def runAlgorithmsOnVectorsWithAnotherVectorization[NV <: GVector[NV]](vectorizationNature: Int, vectorizationIDs: Seq[Int], algorithms: AlgorithmsRestrictions[NV]*)(implicit ct: ClassTag[Cz[ID, O, NV]]): Self[NV] = {
        applyDifferentVectorizationsOfSameNature(vectorizationNature, vectorizationIDs, algorithms, new VMapping[Int, NV])
    }

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

    // def groupedByClusterID(clusteringNumber: Int)(implicit ct: ClassTag[Cz[ID, O, V]]): Collection[(Int, mutable.ArrayBuffer[Cz[ID,O,V]])]

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
trait ClustersIndicesAnalysis[
    ID,
    O,
    V <: GVector[V],
    Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
    Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] with ClusteringCommons {
	import org.clustering4ever.enums.InternalsIndices._
	import org.clustering4ever.enums.ExternalsIndices._
	/**
	 *
	 */
	type Self <: ClustersIndicesAnalysis[ID, O, V, Cz, Collection]
	/**
	 *
	 */
	val internalsIndicesByClusteringNumber = mutable.HashMap.empty[(Int, ClusterID, InternalsIndicesType), Double]
	// val internalsIndicesByClusteringNumber = mutable.HashMap.empty[Int, HMap[InternalsIndicesType, (Double, Distance[_])]]
	/**
	 *
	 */
	val externalsIndicesByClusteringNumber = mutable.HashMap.empty[Int, Map[ExternalsIndicesType, Double]]
	/**
	 *
	 */
	def obtainInternalsIndices[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: Int = 0): Map[InternalsIndicesType, Double]
    /**
     * Compute given internals indices and add result to internalsIndicesByClusteringNumber
     * @return A Map which link internal indices to its associate value 
     */
    def saveInternalsIndices[D <: Distance[V]](metric: D, indices: InternalsIndicesType*)(clusteringNumber: Int = 0): Self
    /**
     *
     */
    def computeInternalsIndicesForEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): Seq[Map[InternalsIndicesType, Double]]
    /**
     *
     */
    def saveInternalsIndicesForEveryClusteringNumber[D <: Distance[V]](metric: D, indices: InternalsIndicesType*): Self
    /**
     * Compute given externals indices and add result to externalsIndicesByClusteringNumber
     * @return A Map which link external indices to its associate value 
     */
    def computeExternalsIndices(groundTruth: Collection[ClusterID], indices: ExternalsIndicesType*)(clusteringNumber: Int = 0): Map[ExternalsIndicesType, Double]
    /**
     *
     */
    def computeExternalsIndicesForEveryClusteringNumber(groundTruth: Collection[ClusterID], indices: ExternalsIndicesType*): Seq[Map[ExternalsIndicesType, Double]]

}