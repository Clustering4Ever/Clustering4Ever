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
import org.clustering4ever.types.MetricIDType._
import org.clustering4ever.types.ClusteringNumberType._
import org.clustering4ever.types.ClusteringInformationTypes._
import org.clustering4ever.types.VectorizationIDTypes._
import org.clustering4ever.enums.InternalsIndices._
import org.clustering4ever.enums.ExternalsIndices._
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
 *
 */
trait CollectionNature[Collection[_]] extends ClusteringCommons
/**
 * The basic trait shared by all clustering models
 */
trait ClusteringModelCz[V <: GVector[V], Collection[_]] extends ClusteringModel with CollectionNature[Collection] {
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
trait ClusteringAlgorithmCz[V <: GVector[V], Collection[_], +CA <: ClusteringArgs, +CM <: ClusteringModelCz[V, Collection]] extends ClusteringAlgorithm with CollectionNature[Collection] {
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
trait DataExplorator[ID, O,	V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_]] extends CollectionNature[Collection] {
	/**
	 *
	 */
	val data: Collection[Cz[ID, O, V]]
}
/**
 *
 */
trait AlgorithmsRestrictions[V <: GVector[V], Collection[_]] {
	/**
	 *
	 */
	type AlgorithmsRestrictions[NV <: GVector[NV]] <: ClusteringAlgorithmCz[NV, Collection, ClusteringArgs, ClusteringModelCz[NV, Collection]]
}
/**
 *
 */
trait ClusteringInformations[Collection[_]] extends CollectionNature[Collection] {

	val clusteringInfo: immutable.Vector[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, Collection])]

	val internalsIndicesByClusteringNumberMetricVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double]

	val externalsIndicesByClusteringNumberVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double]

}
/**
 *
 */
class ClusteringInformationsLocal[GS[X] <: GenSeq[X]](
	val clusteringInfo: immutable.Vector[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, GS])] = immutable.Vector.empty[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, GS])],
	val internalsIndicesByClusteringNumberMetricVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double] = immutable.Map.empty[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double],
	val externalsIndicesByClusteringNumberVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double] = immutable.Map.empty[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double]
) extends CollectionNature[GS]
/**
 *
 */
trait ClusteringChaining[
	ID,
	O,
	V <: GVector[V],
	Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
	Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] with AlgorithmsRestrictions[V, Collection] {
	/**
	 *
	 */
	type Self[NV <: GVector[NV]] <: ClusteringChaining[ID, O, NV, Cz, Collection]
    /**
     *
     */
    val initialVectorNatureMapping = new VMapping[VectorizationID, V]
	/**
	 * A securty value in order to allow proper reduce of Chaining models
	 */
	protected val fusionChainableSecurity: Int
	/**
	 *
	 */
	val chainableID: Int
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
	val clusteringInfo: immutable.Vector[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs, ClusteringModelCz[_, Collection])]
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
	 * @return new ClusteringChaining object and the VectorizationMapping to extract a specific vectorization
	 */
    def addAnyVectorizationNature[NV <: GVector[NV]](vectorizationNature: VectorizationNature, vectorizationID: VectorizationID, towardNewVector: O => NV): (Self[V], VectorizationMapping[VectorizationNature, immutable.Map[VectorizationID, IthVectorization[O, NV]]])
	/**
	 *
	 */
	def addDefaultVectorizationNature(vectorizationID: VectorizationID, towardNewVector: O => V): (Self[V], VectorizationMapping[VectorizationNature, immutable.Map[VectorizationID, IthVectorization[O, V]]]) = addAnyVectorizationNature(0, vectorizationID, towardNewVector)
	/**
	 *
	 */
    def updtV[NV <: GVector[NV]](vectorizationNature: VectorizationNature, vectorizationID: VectorizationID, vMapping: VMapping[VectorizationID, NV] = new VMapping[VectorizationID, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): Self[NV]
	/**
	 *vectorizations
	 */
    def updtVDefaultNature[NV <: GVector[NV]](vectorizationID: VectorizationID, vMapping: VMapping[VectorizationID, NV] = new VMapping[VectorizationID, NV])(implicit ct: ClassTag[Cz[ID, O, NV]]): Self[NV] = {
    	updtV(0, vectorizationID, vMapping)
    }
    /**
     *
     */
    protected def applyDifferentVectorizationsOfSameNature[NV <: GVector[NV]](
    	vectorizationNature: VectorizationNature,
        vectorizationIDs: Seq[VectorizationID],
        algorithms: Seq[AlgorithmsRestrictions[NV]],
        otherVectorizationMapping: VMapping[VectorizationID, NV] = new VMapping[VectorizationID, NV]
    )(implicit ct: ClassTag[Cz[ID, O, NV]]): Self[NV]
    /**
     *
     */
    def runAlgorithmsOnVectorsWithCurrentVectorization(vectorizationIDs: Seq[VectorizationID], algorithms: AlgorithmsRestrictions[V]*)(implicit ct: ClassTag[Cz[ID, O, V]]): Self[V] = {
        applyDifferentVectorizationsOfSameNature(0, vectorizationIDs, algorithms, initialVectorNatureMapping)
    }
    /**
     *
     */
    def runAlgorithmsOnVectorsWithAnotherVectorization[NV <: GVector[NV]](vectorizationNature: VectorizationNature, vectorizationIDs: Seq[Int], algorithms: AlgorithmsRestrictions[NV]*)(implicit ct: ClassTag[Cz[ID, O, NV]]): Self[NV] = {
        applyDifferentVectorizationsOfSameNature(vectorizationNature, vectorizationIDs, algorithms, new VMapping[VectorizationID, NV])
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
    Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] {
    /**
     *
     */
	type DistanceRestriction <: Distance[V]
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
    def centroids[D <: DistanceRestriction](metric: D, clusteringNumber: Int): immutable.Map[ClusterID, V]
    /**
     *
     */
    val centroidsByClusteringNumber = mutable.HashMap.empty[(ClusteringNumber, MetricID), immutable.Map[Int, V]]

}
