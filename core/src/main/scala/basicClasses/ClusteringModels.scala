package org.clustering4ever.clustering
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{GenSeq, immutable}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.GVector
/**
 * The basic trait shared by all clustering models
 */
trait ClusteringModel extends ClusteringCommons {
	// val clusteringStats: ClusteringStats
}
/**
 * The basic trait shared by all clustering models
 */
trait ClusteringModelCz[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], Collection[_], +CA <: ClusteringArgs[V]] extends ClusteringModel with CollectionNature[Collection] {
	/**
	 *
	 */
	implicit val ct: ClassTag[Cz[ID, O, V]]
	/**
	 *
	 */
	val args: CA
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
trait ClusteringModelLocal[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X], +CA <: ClusteringArgs[V]] extends ClusteringModelCz[ID, O, V, Cz, GS, CA] {
	/**
	 *
	 */
	def obtainClusteringIDs(data: GS[Cz[ID, O, V]]): GS[ClusterID] = {
		obtainClustering(data).map(_.clusterIDs.last).asInstanceOf[GS[ClusterID]]
	}
}
