package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, mutable, Map}
import shapeless._
import org.clustering4ever.scala.clusterizables.{Clusterizable, IdentifiedRawObject}
import org.clustering4ever.scala.vectors.{GVector, ScalarVector, BinaryVector}
import org.clustering4ever.shapelesslinked.VMapping
import org.clustering4ever.math.distances.Distance
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
 * The basic trait shared by all clustering algorithms
 */
trait ClusteringAlgorithm extends ClusteringCommons
/**
 * The basic trait shared by all clustering algorithms
 */
trait ClusteringAlgorithmGen[V <: GVector, Collection[_]] extends ClusteringAlgorithm {
	/**
	 * Execute the corresponding clustering algorithm
	 * @return ClusteringModel
	 */
	def run[
		ID,
		O,
		Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz]
	](data: Collection[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): ClusteringModel
}
/**
 * The basic trait shared by all local clustering algorithms
 */
trait LocalClusteringAlgorithm[V <: GVector, GS[X] <: GenSeq[X]] extends ClusteringAlgorithmGen[V, GS]
/**
 * Statistic obtained during clustering algorithm executions
 */
trait ClusteringStats extends ClusteringCommons
/**
 * Neccessary clustering algorithm arguments to launch it 
 */
trait ClusteringArgs extends Serializable
/**
 * Generic concept of data which is a Collection (distributed or not) of Clusterizable
 */
trait DataExplorator[
	ID,
	O,
	V <: GVector,
	Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
	Collection[_]
] {
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
	V <: GVector,
	Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
	Collection[_],
	Self <: ClusteringChaining[ID, O, _, Cz, Collection, Self]
] extends DataExplorator[ID, O, V, Cz, Collection] {
	/**
	 *
	 */
	val clusteringInfo = mutable.ArrayBuffer.empty[(ClusteringArgs, ClusteringModel)]
	/**
	 *
	 */
	// def runAlgorithms[CAG[GV <: GVector, Coll[_]] <: ClusteringAlgorithmGen[GV, Coll]](algorithmsAndArgs: (CAG[V, Collection], ClusteringArgs)*)(implicit ct: ClassTag[Cz[ID, O, V]]): Seq[(ClusteringArgs, ClusteringModel)] = {

	// 	val models = algorithmsAndArgs.map(_._1.run(data))
		
	// 	val res = algorithmsAndArgs.zip(models).map{ case ((enum, args), model) => (args, model) }
	// 	clusteringInfo ++= res
	// 	res
	// }
	/**
	 *
	 */
	def newVectorization[NV <: GVector](vectorizationID: Int, towardNewVector: O => NV): Self
	/**
	 *
	 */
	def updtWorkingVector[GV <: GVector](vectorizationID: Int)(implicit vMapping: VMapping[Int, GV] = new VMapping[Int, GV]): Self

}
/**
 *
 */
trait ScalarDataExplorator[
    ID,
    O,
    V <: Seq[Double],
    Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
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
    Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
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
    V <: GVector,
    Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
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