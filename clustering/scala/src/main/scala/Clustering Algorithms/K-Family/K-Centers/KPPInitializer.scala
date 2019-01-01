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
import org.clustering4ever.scala.vectors.GVector
/**
 *
 */
object KPPInitializer {
	/**
	 * K++ initialization
	 */
	def kppInit[
		ID,
		O,
		V <: GVector : ClassTag,
		Cz[X, Y, Z <: GVector] <: Clusterizable[X, Y, Z, Cz],
		D <: Distance[V]
	](data: GenSeq[Cz[ID, O, V]], metric: D, k: Int) = {

		def obtainNearestCenter(v: V, centers: mutable.ArrayBuffer[V]): V = centers.minBy(metric.d(_, v))
		
		val centersBuff = mutable.ArrayBuffer(data(Random.nextInt(data.size)).workingVector)

		(1 until k).foreach( i => centersBuff += Stats.obtainCenterFollowingWeightedDistribution[V]{
			data.map{ cz =>
				val toPow2 = metric.d(cz.workingVector, obtainNearestCenter(cz.workingVector, centersBuff))
				(cz.workingVector, toPow2 * toPow2)
			}.toBuffer
		} )

		val centers = mutable.HashMap(centersBuff.zipWithIndex.map{ case (center, clusterID) => (clusterID, center) }:_*)
		centers
	}
}