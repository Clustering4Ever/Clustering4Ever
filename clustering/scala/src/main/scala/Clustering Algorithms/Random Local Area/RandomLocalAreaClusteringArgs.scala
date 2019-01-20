package org.clustering4ever.scala.clustering.rla
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.clustering4ever.clustering.GenericClusteringModel
import scala.collection.{mutable, GenSeq}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.models.{CenterOrientedModelLocal, KnnModelModelLocal, CenterOrientedModelLocalClusterizable, KnnModelModelLocalClusterizable}
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.clustering.ClusteringModelLocal
import org.clustering4ever.clustering.ClusteringArgsLocal
/**
 *
 */
trait RLAArgsAncestor[V <: GVector[V], D <: Distance[V]] extends ClusteringArgsLocal[V] {
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
	val algorithm = org.clustering4ever.extensibleAlgorithmNature.RLA
}
/**
 *
 */
case class RLAArgs[V <: GVector[V], D[X <: GVector[X]] <: Distance[X]](val metric: D[V], val epsilon: Double) extends RLAArgsAncestor[V, D[V]] {
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): RLA[ID, O, V, Cz, D, GS] = {
		RLA[ID, O, V, Cz, D, GS](this)
	}
}
/**
 *
 */
case class RLAArgsScalar[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val metric: D[V], val epsilon: Double) extends RLAArgsAncestor[ScalarVector[V], D[V]] {
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, ScalarVector[V]]])(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]): RLAScalar[ID, O, V, Cz, D, GS] = {
		RLAScalar[ID, O, V, Cz, D, GS](this)
	}
}
/**
 *
 */
case class RLAArgsBinary[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val metric: D[V], val epsilon: Double) extends RLAArgsAncestor[BinaryVector[V], D[V]] {
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, BinaryVector[V]]])(implicit ct: ClassTag[Cz[ID, O, BinaryVector[V]]]): RLABinary[ID, O, V, Cz, D, GS] = {
		RLABinary[ID, O, V, Cz, D, GS](this)
	}
}
/**
 *
 */
case class RLAArgsMixt[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y]](val metric: D[Vb, Vs], val epsilon: Double) extends RLAArgsAncestor[MixtVector[Vb, Vs], D[Vb, Vs]] {
	/**
	 *
	 */
	def obtainAlgorithm[ID, O, Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, MixtVector[Vb, Vs]]])(implicit ct: ClassTag[Cz[ID, O, MixtVector[Vb, Vs]]]): RLAMixt[ID, O, Vb, Vs, Cz, D, GS] = {
		RLAMixt[ID, O, Vb, Vs, Cz, D, GS](this)
	}
}
