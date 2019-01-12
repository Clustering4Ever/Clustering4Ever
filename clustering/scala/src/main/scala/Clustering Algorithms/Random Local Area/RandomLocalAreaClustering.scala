package org.clustering4ever.scala.clustering.rla
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.clustering4ever.clustering.{ClusteringAlgorithm, LocalClusteringAlgorithm}
import org.clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance, MixtDistance}
import org.clustering4ever.util.SumVectors
import scala.math.{min, max}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import org.clustering4ever.vectors.{GVector, ScalarVector, BinaryVector, MixtVector}
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.ClusteringArgs
/**
 *
 */
trait RLAArgsTrait[V <: GVector[V], D <: Distance[V]] extends ClusteringArgs {
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
case class RLAArgs[V <: GVector[V], D <: Distance[V]](val metric: D, val epsilon: Double) extends RLAArgsTrait[V, D]
/**
 *
 */
case class RLAArgsReal[V <: Seq[Double], D[X <: Seq[Double]] <: ContinuousDistance[X]](val metric: D[V], val epsilon: Double) extends RLAArgsTrait[ScalarVector[V], D[V]]
/**
 *
 */
case class RLAArgsBinary[V <: Seq[Int], D[X <: Seq[Int]] <: BinaryDistance[X]](val metric: D[V], val epsilon: Double) extends RLAArgsTrait[BinaryVector[V], D[V]]
/**
 *
 */
case class RLAArgsMixt[Vb <: Seq[Int], Vs <: Seq[Double], D[X <: Seq[Int], Y <: Seq[Double]] <: MixtDistance[X, Y]](val metric: D[Vb, Vs], val epsilon: Double) extends RLAArgsTrait[MixtVector[Vb, Vs], D[Vb, Vs]]
/**
 * The random Local Area clustering algorithm introduce at https://ieeexplore.ieee.org/document/7727595
 * @param data : a GenSeq of any type
 * @param epsilon : distance from random selected point under which we consider dots belongs to the same cluster
 * @param metric : a dissimilarity measure associated to O
 */
class RLA[ID, O, V <: GVector[V], Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz], D <: Distance[V], GS[X] <: GenSeq[X], Args <: RLAArgsTrait[V, D]](val args: Args)(implicit val ct: ClassTag[Cz[ID, O, V]]) extends LocalClusteringAlgorithm[ID, O, V, Cz, GS, Args, RLAModel[ID, O, V, Cz, D, GS]] {
	/**
	 *
	 */
	def run(data: GS[Cz[ID, O, V]]): RLAModel[ID, O, V, Cz, D, GS] = {
		@annotation.tailrec
		def go(data: GS[Cz[ID, O, V]], medoids: mutable.HashMap[Int, V], clusterID: Int): mutable.HashMap[Int, V] = {
			if(!data.isEmpty) {
				val randomMedoid = data.head
				val toTreat = data.filterNot( cz => args.metric.d(randomMedoid.v, cz.v) <= args.epsilon ).asInstanceOf[GS[Cz[ID, O, V]]]
				medoids += ((clusterID, randomMedoid.v))
				if(!data.isEmpty) go(toTreat, medoids, clusterID + 1)
				else medoids
			}
			else medoids
		}

		new RLAModel[ID, O, V, Cz, D, GS](go(data, mutable.HashMap.empty[Int, V], 0), args.metric)
	}
}
/**
 * Compagnion object to run the algorithm effortlessly
 */
object RLA {
	/**
	 *
	 */
	def run[
		ID,
		O,
		V <: GVector[V],
		D <: Distance[V],
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
		GS[X] <: GenSeq[X]
	](data: GS[Cz[ID, O, V]], metric: D, epsilon: Double)(implicit ct: ClassTag[Cz[ID, O, V]]): RLAModel[ID, O, V, Cz, D, GS] = {
		(new RLA[ID, O, V, Cz, D, GS, RLAArgs[V, D]](new RLAArgs(metric, epsilon))).run(data)
	}
}