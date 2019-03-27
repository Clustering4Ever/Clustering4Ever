package org.clustering4ever.clustering.rla
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.clustering4ever.clustering.{ClusteringAlgorithm, ClusteringAlgorithmLocal, ClusteringAlgorithmLocalScalar, ClusteringAlgorithmLocalBinary}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixedDistance}
import org.clustering4ever.util.SumVectors
import scala.math.{min, max}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixedVector}
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.ScalaCollectionImplicits._
import org.clustering4ever.clustering.arguments.MetricArgs
/**
 * The random Local Area clustering algorithm introduce at https://ieeexplore.ieee.org/document/7727595
 * @param data a GenSeq of any type
 * @param epsilon distance from random selected point under which we consider dots belongs to the same cluster
 * @param metric a dissimilarity measure associated to V
 */
trait RLAAncestor[V <: GVector[V], D <: Distance[V], CM <: RLAModelAncestor[V, D]] extends ClusteringAlgorithmLocal[V, CM] with MetricArgs[V, D] {
	/**
	 * The radius explored around selected medoid
	 */
	val epsilon: Double
	/**
	 *
	 */
	protected final def obtainCenters[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): immutable.HashMap[Int, V] = {
		@annotation.tailrec
		def go(data: GS[Cz[O, V]], medoids: immutable.HashMap[Int, V], clusterID: Int): immutable.HashMap[Int, V] = {
			if(!data.isEmpty) {
				val randomMedoid = data.head
				val toTreat = data.filterNot( cz => metric.d(randomMedoid.v, cz.v) <= epsilon ).asInstanceOf[GS[Cz[O, V]]]
				if(!data.isEmpty) go(toTreat, medoids + ((clusterID, randomMedoid.v)), clusterID + 1)
				else medoids
			}
			else medoids
		}
		go(data, immutable.HashMap.empty[Int, V], 0)
	}
}
/**
 *
 */
final case class RLA[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val metric: D[V], val epsilon: Double) extends RLAAncestor[V, D[V], RLAModel[V, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.RLA

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, V]]): RLAModel[V, D] = {
		RLAModel[V, D](metric, epsilon, obtainCenters(data))
	}
}
/**
 *
 */
final case class RLAScalar[D <: ContinuousDistance](final val metric: D, final val epsilon: Double) extends ClusteringAlgorithmLocalScalar[RLAModelScalar[D]] with RLAAncestor[ScalarVector, D, RLAModelScalar[D]] {
	
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.RLAScalar

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, ScalarVector]]): RLAModelScalar[D] = {
		RLAModelScalar[D](metric, epsilon, obtainCenters(data))
	}

}
/**
 *
 */
object RLAScalar {
	/**
	 *
	 */
	final def fit[D <: ContinuousDistance, GS[Y] <: GenSeq[Y]](data: GS[Array[Double]], metric: D, epsilon: Double): RLAModelScalar[D] = {
		RLAScalar(metric, epsilon).fit(scalarToClusterizable(data))
	}
}
/**
 *
 */
final case class RLABinary[D <: BinaryDistance](final val metric: D, final val epsilon: Double) extends ClusteringAlgorithmLocalBinary[RLAModelBinary[D]] with RLAAncestor[BinaryVector, D, RLAModelBinary[D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.RLABinary

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, BinaryVector]]): RLAModelBinary[D] = {
		RLAModelBinary[D](metric, epsilon, obtainCenters(data))
	}
}
/**
 *
 */
final case class RLAMixed[D <: MixedDistance](final val metric: D, final val epsilon: Double) extends RLAAncestor[MixedVector, D, RLAModelMixed[D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.RLAMixed

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, MixedVector]]): RLAModelMixed[D] = {
		RLAModelMixed[D](metric, epsilon, obtainCenters(data))
	}
}