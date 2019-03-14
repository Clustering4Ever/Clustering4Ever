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
final case class RLAScalar[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](final val metric: D[V], final val epsilon: Double) extends ClusteringAlgorithmLocalScalar[V, RLAModelScalar[V, D]] with RLAAncestor[ScalarVector[V], D[V], RLAModelScalar[V, D]] {
	
	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.RLAScalar

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, ScalarVector[V]]]): RLAModelScalar[V, D] = {
		RLAModelScalar[V, D](metric, epsilon, obtainCenters(data))
	}

}
/**
 *
 */
object RLAScalar {
	/**
	 *
	 */
	final def fit[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[Y] <: GenSeq[Y]](data: GS[V], metric: D[V], epsilon: Double): RLAModelScalar[V, D] = {
		RLAScalar(metric, epsilon).fit(scalarToClusterizable(data))
	}
}
/**
 *
 */
final case class RLABinary[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](final val metric: D[V], final val epsilon: Double) extends ClusteringAlgorithmLocalBinary[V, RLAModelBinary[V, D]] with RLAAncestor[BinaryVector[V], D[V], RLAModelBinary[V, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.RLABinary

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, BinaryVector[V]]]): RLAModelBinary[V, D] = {
		RLAModelBinary[V, D](metric, epsilon, obtainCenters(data))
	}
}
/**
 *
 */
final case class RLAMixed[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixedDistance[X, Y]](final val metric: D[Vb, Vs], final val epsilon: Double) extends RLAAncestor[MixedVector[Vb, Vs], D[Vb, Vs], RLAModelMixed[Vb, Vs, D]] {

	final val algorithmID = org.clustering4ever.extensibleAlgorithmNature.RLAMixed

	final def fit[O, Cz[B, C <: GVector[C]] <: Clusterizable[B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[O, MixedVector[Vb, Vs]]]): RLAModelMixed[Vb, Vs, D] = {
		RLAModelMixed[Vb, Vs, D](metric, epsilon, obtainCenters(data))
	}
}