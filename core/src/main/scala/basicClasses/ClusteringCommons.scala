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
import org.clustering4ever.shapeless.{VMapping, VectorizationMapping, DistancesMapping}
import org.clustering4ever.vectorizables.Vectorizable
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectorizations.{EmployedVectorization, IthVectorization}
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
trait ClusteringModelCz[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_]] extends ClusteringModel with CollectionNature[Collection] {
	/**
	 *
	 */
	implicit val ct: ClassTag[Cz[ID, O, V]]
	/**
	 * General methods to obtain a clustering from the model in order to measure performances scores
	 */
	def obtainClustering(data: Collection[Cz[ID, O, V]]): Collection[Cz[ID, O, V]]
	/**
	 * Obtain only clusterIDs
	 */
	def obtainClusteringIDs(data: Collection[Cz[ID, O, V]]): Collection[ClusterID]
}
/**
 *
 */
trait ClusteringModelLocal[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]] extends ClusteringModelCz[ID, O, V, Cz, GS] {
	/**
	 *
	 */
	def obtainClusteringIDs(data: GS[Cz[ID, O, V]]): GS[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last).asInstanceOf[GS[ClusterID]]
	}
}
/**
 * The basic trait shared by all clustering algorithms
 */
trait ClusteringAlgorithm extends ClusteringCommons
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
 * The basic trait shared by all clustering algorithms working on Clusterizable
 */
trait ClusteringAlgorithmCz[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_], +CA <: ClusteringArgs[V], +CM <: ClusteringModelCz[ID, O, _, Cz, Collection]] extends ClusteringAlgorithm {
	/**
	 *
	 */
	implicit val ct: ClassTag[Cz[ID, O, V]]
	/**
	 *
	 */
	val args: CA
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run(data: Collection[Cz[ID, O, V]]): CM

}
/**
 * The basic trait shared by all local clustering algorithms
 */
trait LocalClusteringAlgorithm[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X], +CA <: ClusteringArgs[V], +CM <: ClusteringModelCz[ID, O, _, Cz, GS]] extends ClusteringAlgorithmCz[ID, O, V, Cz, GS, CA, CM]
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
	type CA <: ClusteringArgs[V]
	/**
	 *
	 */
	val algorithm: ClusteringAlgorithmNature
}
/**
 *
 */
trait ClusteringArgsLocal[V <: GVector[V]] extends ClusteringArgs[V] {
	/**
	 *
	 */
	type CM[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]] <: ClusteringModelLocal[ID, O, V, Cz, GS]
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): LocalClusteringAlgorithm[ID, O, V, Cz, GS, CA, CM[ID, O, Cz, GS]]
}
/**
 *
 */
trait AlgorithmsRestrictions[ID, O,	V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_]] {
	/**
	 *
	 */
	type AlgorithmsRestrictions[NV <: GVector[NV]] <: ClusteringAlgorithmCz[ID, O, NV, Cz, Collection, ClusteringArgs[NV], ClusteringModelCz[ID, O, NV, Cz, Collection]]
}
/**
 *
 */
trait ClusteringInformations[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_]] extends CollectionNature[Collection] {

	val clusteringInformations: immutable.Vector[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs[_], ClusteringModelCz[ID, O, _, Cz, Collection])]

	// val internalsIndicesByClusteringNumberMetricVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double] =
	// 	immutable.Map.empty[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double]

	// val externalsIndicesByClusteringNumberVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double] =
	// 	immutable.Map.empty[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double]

}
/**
 *
 */
case class ClusteringInformationsLocal[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](
	val clusteringInformations: immutable.Vector[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs[_], ClusteringModelCz[ID, O, _, Cz, GS])] =
		immutable.Vector.empty[(GlobalClusteringRunNumber, EmployedVectorization, ClusteringArgs[_], ClusteringModelCz[ID, O, _, Cz, GS])]
	// val internalsIndicesByClusteringNumberMetricVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double] =
	// 	immutable.Map.empty[(GlobalClusteringRunNumber, MetricID, VectorizationID, InternalsIndicesType), Double],
	// val externalsIndicesByClusteringNumberVectorizationIDIndex: immutable.Map[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double] =
	// 	immutable.Map.empty[(GlobalClusteringRunNumber, VectorizationID, ExternalsIndicesType), Double]
) extends ClusteringInformations[ID, O, Cz, GS]
/**
 *
 */
trait ClusteringChaining[
	ID,
	O,
	V <: GVector[V],
	Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
	Collection[_]
] extends DataExplorator[ID, O, V, Cz, Collection] with AlgorithmsRestrictions[ID, O, V, Cz, Collection] {
	/**
	 *
	 */
	implicit val ct: ClassTag[Cz[ID, O, V]]
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
	val clusteringInfo: ClusteringInformations[ID, O, Cz, Collection]
	/**
	 * Total number of algorithms launched
	 */
	val globalClusteringRunNumber: GlobalClusteringRunNumber = 0
	/**
	 * Run one algorithm on the current vectorization
	 */
	def runAlgorithm(algorithm: AlgorithmsRestrictions[V]): Self[V]
    /**
	 * Run multiples algorithms on the current vectorization
     */
    def runAlgorithms(algorithms: AlgorithmsRestrictions[V]*): Self[V]
   	/**
	 * @return new ClusteringChaining object with a vector of type NV and the VectorizationMapping to extract the specific vectorization
	 */
    def addVectorizationNature[NV <: GVector[NV]](vectorizationNature: VectorizationNature, vectorizationID: VectorizationID, towardNewVector: O => NV): (Self[V], VectorizationMapping[VectorizationNature, immutable.Map[VectorizationID, IthVectorization[O, NV]]])
	/**
	 * @return new ClusteringChaining object with a vector of type same nature than current one and the VectorizationMapping to extract the specific vectorization
	 */
	def addVectorizationNature(vectorizationID: VectorizationID, towardNewVector: O => V): (Self[V], VectorizationMapping[VectorizationNature, immutable.Map[VectorizationID, IthVectorization[O, V]]]) = addVectorizationNature(currentVectorization.vectorizationNature, vectorizationID, towardNewVector)
	/**
	 * Actualize the data set with a new vector of any nature
	 */
    def updateVector[NV <: GVector[NV]](vectorizationNature: VectorizationNature, vectorizationID: VectorizationID)(implicit ct: ClassTag[Cz[ID, O, NV]]): Self[NV]
	/**
	 * Actualize the data set with a new vector of the same nature than previous one
	 */
    def updateVector(vectorizationID: VectorizationID): Self[V] = {
    	updateVector[V](currentVectorization.vectorizationNature, vectorizationID)
    }
    /**
     *
     */
    def runAlgorithmsOnMultipleVectorizations(
        vectorizationIDs: Seq[VectorizationID],
        algorithms: AlgorithmsRestrictions[V]*
    ): Self[V]

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
