package org.clustering4ever.clustering.scala.epsilonproximity
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
import org.clustering4ever.utils.SortGsCz
/**
 *
 */
trait EpsilonProximityModelAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringModelLocal[V] with DataBasedModel[V, D] {
	/**
	 * The value epsilon given/determined by the epsilon proximity algorithm
	 */
	val epsilon: Double
	/**
	 *
	 */
	protected val inputDataHashCode: Int

	protected[clustering] def obtainClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): GS[Cz[O, V]] = {
		data.zip(datapointWithClusterIDSortedByPointID).map{ case (cz, (_, (v, clusterID))) => cz.addClusterIDs(clusterID) }.asInstanceOf[GS[Cz[O, V]]]
	}
	/**
	 * This method work only with input dataset which generate this model, please use others method for new set of points 
	 *
	 * @return the clusterized dataset
	 */
	def obtainInputDataClustering[O, Cz[Y, Z <: GVector[Z]] <: Clusterizable[Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]], isDatasetSortedByID: Boolean = false): GS[Cz[O, V]] = {
		require(inputDataHashCode == data.hashCode, println("This method work only with input dataset which generate this model, please use others method for new set of points"))
	    val dataSortedByID: GS[Cz[O, V]] = if(!isDatasetSortedByID) SortGsCz.sortByID(data) else data
		obtainClustering(dataSortedByID)
	}
}
/**
 *
 */
case class EpsilonProximityModel[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val datapointWithClusterIDSortedByPointID: mutable.ArrayBuffer[(Long, (V, Int))], val epsilon: Double, val metric: D[V], protected val inputDataHashCode: Int) extends EpsilonProximityModelAncestor[V, D[V]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximity
}
/**
 *
 */
case class EpsilonProximityModelScalar[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val datapointWithClusterIDSortedByPointID: mutable.ArrayBuffer[(Long, (ScalarVector[V], Int))], val epsilon: Double, val metric: D[V], protected val inputDataHashCode: Int) extends EpsilonProximityModelAncestor[ScalarVector[V], D[V]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximityScalar
}
/**
 *
 */
case class EpsilonProximityModelBinary[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val datapointWithClusterIDSortedByPointID: mutable.ArrayBuffer[(Long, (BinaryVector[V], Int))], val epsilon: Double, val metric: D[V], protected val inputDataHashCode: Int) extends EpsilonProximityModelAncestor[BinaryVector[V], D[V]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximityBinary
}
/**
 *
 */
case class EpsilonProximityModelMixt[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixedDistance[X, Y]](val datapointWithClusterIDSortedByPointID: mutable.ArrayBuffer[(Long, (MixedVector[Vb, Vs], Int))], val epsilon: Double, val metric: D[Vb, Vs], protected val inputDataHashCode: Int) extends EpsilonProximityModelAncestor[MixedVector[Vb, Vs], D[Vb, Vs]] {
	val algorithmID = org.clustering4ever.extensibleAlgorithmNature.EpsilonProximityMixt
}