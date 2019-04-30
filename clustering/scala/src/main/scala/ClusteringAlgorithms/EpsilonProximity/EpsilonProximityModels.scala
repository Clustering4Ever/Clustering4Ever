package org.clustering4ever.clustering.epsilonproximity.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import scala.collection.{immutable, mutable, Traversable, GenSeq}
import org.clustering4ever.clustering.ClusteringModelLocal
import org.clustering4ever.math.distances.{GenericDistance, Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.models.DataBasedModel
import org.clustering4ever.identifiables.IdentifiedRawObject
import org.clustering4ever.utils.SortingTools
/**
 *
 */
trait EpsilonProximityModelAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringModelLocal[V] with DataBasedModel[V, D] {
	/**
	 * The value epsilon given/determined by the epsilon proximity algorithm
	 */
	val epsilon: Double
	/**
	 * The number of cluster found
	 */
	val clusterNumber: Int
	/**
	 *
	 */
	protected val inputDataHashCode: Int

	protected[clustering] final def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {
		data.zip(datapointWithClusterIDSortedByPointID).map{ case (cz, (_, (v, clusterID))) => cz.addClusterIDs(clusterID) }.asInstanceOf[GS[Cz[O, V]]]
	}
	/**
	 * This method work only with input dataset which generate this model, please use others method for new set of points 
	 *
	 * @return the clusterized dataset
	 */
	final def obtainInputDataClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]], isDatasetSortedByID: Boolean = false): GS[Cz[O, V]] = {
		require(inputDataHashCode == data.hashCode, println("This method work only with input dataset which generate this model, please use others method for new set of points"))
	    val dataSortedByID: GS[Cz[O, V]] = if(!isDatasetSortedByID) SortingTools.sortByID(data) else data
		obtainClustering(dataSortedByID)
	}
}
/**
 *
 */
final case class EpsilonProximityModel[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](final val datapointWithClusterIDSortedByPointID: mutable.ArrayBuffer[(Long, (V, Int))], final val epsilon: Double, final val metric: D[V], final val clusterNumber: Int, protected final val inputDataHashCode: Int) extends EpsilonProximityModelAncestor[V, D[V]] {
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximity
}
/**
 *
 */
final case class EpsilonProximityModelScalar[D <: ContinuousDistance](final val datapointWithClusterIDSortedByPointID: mutable.ArrayBuffer[(Long, (ScalarVector, Int))], final val epsilon: Double, final val metric: D, final val clusterNumber: Int, protected final val inputDataHashCode: Int) extends EpsilonProximityModelAncestor[ScalarVector, D] {
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximityScalar
}
/**
 *
 */
final case class EpsilonProximityModelBinary[D <: BinaryDistance](final val datapointWithClusterIDSortedByPointID: mutable.ArrayBuffer[(Long, (BinaryVector, Int))], final val epsilon: Double, final val metric: D, final val clusterNumber: Int, protected final val inputDataHashCode: Int) extends EpsilonProximityModelAncestor[BinaryVector, D] {
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximityBinary
}
/**
 *
 */
final case class EpsilonProximityModelMixed[D <: MixedDistance](final val datapointWithClusterIDSortedByPointID: mutable.ArrayBuffer[(Long, (MixedVector, Int))], final val epsilon: Double, final val metric: D, final val clusterNumber: Int, protected final val inputDataHashCode: Int) extends EpsilonProximityModelAncestor[MixedVector, D] {
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximityMixed
}