package clustering4ever.scala.clustering.kmodes

import _root_.scala.collection.{immutable, mutable}
import _root_.scala.util.Random
import _root_.clustering4ever.math.distances.BinaryDistance
import _root_.clustering4ever.util.SumArrays
import _root_.clustering4ever.clustering.datasetstype.ClusteringTypes
import _root_.clustering4ever.clustering.ClusteringAlgorithms

class KModes(
	data: Array[(Int, Array[Int])],
	var k: Int,
	var epsilon: Double,
	var maxIter: Int,
	var metric: BinaryDistance
) extends ClusteringAlgorithms[Int, Int, Array[(Int, (Int, Array[Int]))]]
{
	val dim = data.head._2.size

	def run(): ClusterizedData =
	{
		val kmodes = mutable.HashMap((for( clusterID <- 0 until k ) yield( (clusterID, Array.fill(dim)(Random.nextInt(2))) )):_*)
		val kmodesCpt = kmodes.map{ case (clusterID, _) => (clusterID, 0) }

		def obtainNearestModID(v: Array[Int]): ClusterID = kmodes.toArray.map{ case(clusterID, mod) => (clusterID, metric.d(mod, v)) }.sortBy(_._2).head._1

		val zeroMod = Array.fill(dim)(0)
		var cpt = 0
		var allModsHaveConverged = false
		while( cpt < maxIter && ! allModsHaveConverged )
		{
			// Allocation to modes
			val clusterized = data.map{ case (id, v) => (id, v, obtainNearestModID(v)) }

			val kModesBeforeUpdate = kmodes.clone

			// Reinitialization of modes
			kmodes.foreach{ case (clusterID, mod) => kmodes(clusterID) = zeroMod }
			kmodesCpt.foreach{ case (clusterID, _) => kmodesCpt(clusterID) = 0 }

			// Updatating Modes
			clusterized.foreach{ case (_, v, clusterID) =>
			{
				kmodes(clusterID) = SumArrays.sumArraysNumerics(kmodes(clusterID), v)
				kmodesCpt(clusterID) += 1
			}}

			kmodes.foreach{ case (clusterID, mod) => kmodes(clusterID) = mod.map( v => if( v * 2 >= kmodesCpt(clusterID) ) 1 else 0 ) }

			allModsHaveConverged = kModesBeforeUpdate.forall{ case (clusterID, previousMod) => metric.d(previousMod, kmodes(clusterID)) <= epsilon }

			cpt += 1
		}

		val finalClustering = data.map{ case (id, v) =>
		{
			val clusterID = obtainNearestModID(v)
			(clusterID, (id, v))
		}}
		finalClustering
	}
	
}

object KModes extends ClusteringTypes[Int, Int, Array[(Int, (Int, Array[Int]))]]
{

	def run(data: Array[(ID, Vector)], k: Int, epsilon: Double, maxIter: Int, metric: BinaryDistance): ClusterizedData =
	{
		val kmodes = new KModes(data, k, epsilon, maxIter, metric)
		val kmodesClusterized = kmodes.run()
		kmodesClusterized
	}
}