package org.clustering4ever.scala.clustering.rla
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.reflect.ClassTag
import org.clustering4ever.clustering.{ClusteringAlgorithm, LocalClusteringAlgorithm}
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.util.SumVectors
import scala.math.{min, max}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
import org.clustering4ever.vectors.GVector
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.clustering.ClusteringArgs
/**
 *
 */
case class RLAArgs[V <: GVector[V], D <: Distance[V]](val metric: D, val epsilon: Double) extends ClusteringArgs {
	override val algorithm = org.clustering4ever.extensibleAlgorithmNature.RLA
}
/**
 * The random Local Area clustering algorithm introduce at https://ieeexplore.ieee.org/document/7727595
 * @param data : a GenSeq of any type
 * @param epsilon : distance from random selected point under which we consider dots belongs to the same cluster
 * @param metric : a dissimilarity measure associated to O
 */
class RLAGen extends ClusteringAlgorithm {
	/**
	 * Run the Random Local Clustering
	 */
	def runGen[O, D <: Distance[O]](data: GenSeq[O], epsilon: Double, metric: D): RLAModel[O, D] = {

		@annotation.tailrec
		def go(data: GenSeq[O], medoids: mutable.HashMap[Int, O], clusterID: Int): mutable.HashMap[Int, O] = {
			if(!data.isEmpty) {
				val randomMedoid = data.head
				val toTreat = data.filterNot( v => metric.d(randomMedoid, v) <= epsilon )
				medoids += ((clusterID, randomMedoid))
				if(!data.isEmpty) go(toTreat, medoids, clusterID + 1)
				else medoids	
			}
			else medoids
		}

		new RLAModel[O, D](go(data, mutable.HashMap.empty[Int, O], 0), metric)
	}
}
/**
 *
 */
class RLA[V <: GVector[V], D <: Distance[V], GS[X] <: GenSeq[X]](val args: RLAArgs[V, D]) extends LocalClusteringAlgorithm[V, GS, RLAArgs[V, D], RLAModelCz[V, D, GS]] {
	/**
	 *
	 */
	def run[
		ID,
		O,
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz]
	](data: GS[Cz[ID, O, V]])(implicit ct: ClassTag[Cz[ID, O, V]]): RLAModelCz[V, D, GS] = {
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

		new RLAModelCz[V, D, GS](go(data, mutable.HashMap.empty[Int, V], 0), args.metric)
	}
}
/**
 * Compagnion object to run the algorithm effortlessly
 */
object RLA {
	/**
	 * Run the RLA
	 */
	def run[O, D <: Distance[O]](data: GenSeq[O], epsilon: Double, metric: D): RLAModel[O, D] = {
		(new RLAGen).runGen(data, epsilon, metric)
	}
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
	](data: GS[Cz[ID, O, V]], metric: D, epsilon: Double)(implicit ct: ClassTag[Cz[ID, O, V]]): RLAModelCz[V, D, GS] = {
		(new RLA[V, D, GS](new RLAArgs(metric, epsilon))).run(data)
	}
}