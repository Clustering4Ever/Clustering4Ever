package org.clustering4ever.clustering.kcenters.scala
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{mutable, immutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import org.clustering4ever.stats.Stats
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.clusterizables.Clusterizable
import org.clustering4ever.vectors.GVector
/**
 *
 */
object KPPInitializer extends Serializable {
	/**
	 * K++ initialization
	 */
	def kppInit[
		ID,
		O,
		V <: GVector[V],
		Cz[X, Y, Z <: GVector[Z]] <: Clusterizable[X, Y, Z, Cz],
		D <: Distance[V]
	](data: GenSeq[Cz[ID, O, V]], metric: D, k: Int): immutable.HashMap[Int, V] = {

		def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
		
		val centersBuff = mutable.ArrayBuffer(data(Random.nextInt(data.size)).v)

		(1 until k).foreach( i => centersBuff += Stats.obtainCenterFollowingWeightedDistribution[V]{
			data.map{ cz =>
				val toPow2 = metric.d(cz.v, obtainNearestCenter(cz.v, centersBuff))
				(cz.v, toPow2 * toPow2)
			}.toBuffer
		} )

		val centers = immutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
}