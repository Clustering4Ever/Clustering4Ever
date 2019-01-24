package org.clustering4ever.scala.clustering.rla
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.clustering4ever.clustering.{ClusteringAlgorithmGeneric, ClusteringAlgorithmLocal}
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
trait RLAAncestor[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], GS[X] <: GenSeq[X], +Args <: RLAArgsAncestor[V, D], +Model <: RLAModelAncestor[ID, O, V, Cz, D, GS, Args]] extends ClusteringAlgorithmLocal[ID, O, V, Cz, GS, Args, Model] {
	/**
	 *
	 */
	val args: Args
	/**
	 *
	 */
	protected implicit val ct: ClassTag[Cz[ID, O, V]]
	/**
	 *
	 */
	protected def obtainCenters(data: GS[Cz[ID, O, V]]): immutable.HashMap[Int, V] = {
		@annotation.tailrec
		def go(data: GS[Cz[ID, O, V]], medoids: immutable.HashMap[Int, V], clusterID: Int): immutable.HashMap[Int, V] = {
			if(!data.isEmpty) {
				val randomMedoid = data.head
				val toTreat = data.filterNot( cz => args.metric.d(randomMedoid.v, cz.v) <= args.epsilon ).asInstanceOf[GS[Cz[ID, O, V]]]
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
case class RLA[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: GVector[X]] <: Distance[X], GS[X] <: GenSeq[X]](val args: RLAArgs[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, V]]) extends RLAAncestor[ID, O, V, Cz, D[V], GS, RLAArgs[V, D], RLAModel[ID, O, V, Cz, D, GS]] {
	/**
	 *
	 */
	def run(data: GS[Cz[ID, O, V]]): RLAModel[ID, O, V, Cz, D, GS] = RLAModel[ID, O, V, Cz, D, GS](obtainCenters(data), args.metric, args)
}
/**
 * Compagnion object to run the algorithm effortlessly
 */
object RLA {
	/**
	 *
	 */
	def run[ID, O, V <: GVector[V], D[X <: GVector[X]] <: Distance[X], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], GS[X] <: GenSeq[X]](data: GS[Cz[ID, O, V]], metric: D[V], epsilon: Double)(implicit ct: ClassTag[Cz[ID, O, V]]): RLAModel[ID, O, V, Cz, D, GS] = {
		(RLA[ID, O, V, Cz, D, GS](RLAArgs(metric, epsilon))).run(data)
	}
}
/**
 *
 */
case class RLAScalar[ID, O, V <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[X] <: GenSeq[X]](val args: RLAArgsScalar[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, ScalarVector[V]]]) extends RLAAncestor[ID, O, ScalarVector[V], Cz, D[V], GS, RLAArgsScalar[V, D], RLAModelScalar[ID, O, V, Cz, D, GS]] {
	/**
	 *
	 */
	def run(data: GS[Cz[ID, O, ScalarVector[V]]]): RLAModelScalar[ID, O, V, Cz, D, GS] = RLAModelScalar[ID, O, V, Cz, D, GS](obtainCenters(data), args.metric, args)
}
/**
 *
 */
object RLAScalar {
	/**
	 *
	 */
	def run[ID, O, V <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[X] <: GenSeq[X]](
		data: GS[Cz[ID, O, ScalarVector[V]]],
		metric: D[V],
		epsilon: Double
	)(implicit ct: ClassTag[Cz[ID, O, ScalarVector[V]]]): RLAModelScalar[ID, O, V, Cz, D, GS] = {
		(RLAScalar[ID, O, V, Cz, D, GS](RLAArgsScalar(metric, epsilon))).run(data)
	}
	/**
	 * Run the K-Means with any continuous distance
	 */
	def run[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X], GS[Y] <: GenSeq[Y]](
		data: GS[V],
		metric: D[V],
		epsilon: Double
	): RLAModelScalar[Int, ScalarVector[V], V, EasyClusterizable, D, GS] = {
		val rlaModel = run(scalarToClusterizable(data), metric, epsilon)
		rlaModel
	}
}
/**
 *
 */
case class RLABinary[ID, O, V <: Seq[Int], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int]] <: BinaryDistance[X], GS[X] <: GenSeq[X]](val args: RLAArgsBinary[V, D])(protected implicit val ct: ClassTag[Cz[ID, O, BinaryVector[V]]]) extends RLAAncestor[ID, O, BinaryVector[V], Cz, D[V], GS, RLAArgsBinary[V, D], RLAModelBinary[ID, O, V, Cz, D, GS]] {
	/**
	 *
	 */
	def run(data: GS[Cz[ID, O, BinaryVector[V]]]): RLAModelBinary[ID, O, V, Cz, D, GS] = RLAModelBinary[ID, O, V, Cz, D, GS](obtainCenters(data), args.metric, args)
}
/**
 *
 */
case class RLAMixt[ID, O, Vb <: Seq[Int], Vs <: Seq[Double], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y], GS[X] <: GenSeq[X]](val args: RLAArgsMixt[Vb, Vs, D])(protected implicit val ct: ClassTag[Cz[ID, O, MixtVector[Vb, Vs]]]) extends RLAAncestor[ID, O, MixtVector[Vb, Vs], Cz, D[Vb, Vs], GS, RLAArgsMixt[Vb, Vs, D], RLAModelMixt[ID, O, Vb, Vs, Cz, D, GS]] {
	/**
	 *
	 */
	def run(data: GS[Cz[ID, O, MixtVector[Vb, Vs]]]): RLAModelMixt[ID, O, Vb, Vs, Cz, D, GS] = RLAModelMixt[ID, O, Vb, Vs, Cz, D, GS](obtainCenters(data), args.metric, args)
}