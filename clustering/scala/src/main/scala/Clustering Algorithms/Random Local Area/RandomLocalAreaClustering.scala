package org.clustering4ever.scala.clustering.rla
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.clustering4ever.clustering.{ClusteringAlgorithmAncestor, ClusteringAlgorithmLocal}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.util.SumVectors
import scala.math.{min, max}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.clusterizables.{Clusterizable, EasyClusterizable}
import org.clustering4ever.util.ScalaCollectionImplicits._
/**
 * The random Local Area clustering algorithm introduce at https://ieeexplore.ieee.org/document/7727595
 * @param data : a GenSeq of any type
 * @param epsilon : distance from random selected point under which we consider dots belongs to the same cluster
 * @param metric : a dissimilarity measure associated to V
 */
trait RLAAncestor[V <: GVector[V], D <: Distance[V], CM <: RLAModelAncestor[V, D]] extends ClusteringAlgorithmLocal[V, CM] {
	/**
	 *
	 */
	val metric: D
	/**
	 *
	 */
	val epsilon: Double
	/**
	 *
	 */
	protected def obtainCenters[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]]): immutable.HashMap[Int, V] = {
		@annotation.tailrec
		def go(data: GS[Cz[ID, O, V]], medoids: immutable.HashMap[Int, V], clusterID: Int): immutable.HashMap[Int, V] = {
			if(!data.isEmpty) {
				val randomMedoid = data.head
				val toTreat = data.filterNot( cz => metric.d(randomMedoid.v, cz.v) <= epsilon ).asInstanceOf[GS[Cz[ID, O, V]]]
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
case class RLA[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val metric: D[V], val epsilon: Double) extends RLAAncestor[V, D[V], RLAModel[V, D]] {
	def run[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]]): RLAModel[V, D] = {
		RLAModel[V, D](metric, epsilon, obtainCenters(data))
	}
}
/**
 *
 */
case class RLAScalar[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val metric: D[V], val epsilon: Double) extends RLAAncestor[ScalarVector[V], D[V], RLAModelScalar[V, D]] {
	def run[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, ScalarVector[V]]]): RLAModelScalar[V, D] = {
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
	def run[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[V],
		metric: D[V],
		epsilon: Double
	)(implicit d: DummyImplicit): RLAModelScalar[V, D] = {
		val rlaArgs = RLAScalar(metric, epsilon).run(scalarToClusterizable(data))
		rlaArgs
	}
}
/**
 *
 */
case class RLABinary[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val metric: D[V], val epsilon: Double) extends RLAAncestor[BinaryVector[V], D[V], RLAModelBinary[V, D]] {
	def run[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, BinaryVector[V]]]): RLAModelBinary[V, D] = {
		RLAModelBinary[V, D](metric, epsilon, obtainCenters(data))
	}
}
/**
 *
 */
case class RLAMixt[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y]](val metric: D[Vb, Vs], val epsilon: Double) extends RLAAncestor[MixtVector[Vb, Vs], D[Vb, Vs], RLAModelMixt[Vb, Vs, D]] {
	def run[ID, O, Cz[A, B, C <: GVector[C]] <: Clusterizable[A, B, C, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, MixtVector[Vb, Vs]]]): RLAModelMixt[Vb, Vs, D] = {
		RLAModelMixt[Vb, Vs, D](metric, epsilon, obtainCenters(data))
	}
}