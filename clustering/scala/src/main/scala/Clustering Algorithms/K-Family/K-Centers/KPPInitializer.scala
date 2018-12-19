package org.clustering4ever.scala.clustering.kcenters
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.{mutable, GenSeq}
import scala.util.Random
import scala.reflect.ClassTag
import org.clustering4ever.stats.Stats
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.scala.clusterizables.Clusterizable
/**
 *
 */
object KPPInitializer {
	/**
	 * K++ initialization
	 */
	def kppInit[
		ID: Numeric,
		O,
		V: ClassTag,
		Cz[ID, O, V] <: Clusterizable[ID, O, V, Cz[ID, O, V]],
		D <: Distance[V]
	](data: GenSeq[Cz[ID, O, V]], metric: D, k: Int)(workingVector: Int = 0) = {

		def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
		
		val centersBuff = mutable.ArrayBuffer(data(Random.nextInt(data.size)).vector(workingVector))

		(1 until k).foreach( i => centersBuff += Stats.obtainCenterFollowingWeightedDistribution[V]{
			data.map{ cz =>
				val toPow2 = metric.d(cz.vector(workingVector), obtainNearestCenter(cz.vector(workingVector), centersBuff))
				(cz.vector(workingVector), toPow2 * toPow2)
			}.toBuffer
		} )

		val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
}