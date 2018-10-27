package clustering4ever.scala.clustering.rla
/**
 * @author Beck Gaël
 */
import clustering4ever.clustering.ClusteringAlgorithms
import clustering4ever.math.distances.Distance
import clustering4ever.util.SumVectors
import scala.math.{min, max}
import scala.collection.{immutable, mutable, GenSeq}
import scala.util.Random
/**
 * The random Local Area clustering algorithm introduce at https://ieeexplore.ieee.org/document/7727595
 * @param data : a GenSeq of any type
 * @param epsilon : distance from random selected point under which we consider dots belongs to the same cluster
 * @param metric : a dissimilarity measure associated to O
 */
class RLA[O](
	data: GenSeq[O],
	epsilon: Double,
	metric: Distance[O]
) extends ClusteringAlgorithms {
	/**
	 * Run the Random Local Clustering
	 */
	def run(): RLAModel[O] = {

		@annotation.tailrec
		def go(data: GenSeq[O], medoids: mutable.HashMap[Int, O], clusterID: Int): mutable.HashMap[Int, O] = {
			if( ! data.isEmpty ) {
				val randomMedoid = data.head
				val toTreat = data.filterNot( v => metric.d(randomMedoid, v) <= epsilon )
				medoids += ((clusterID, randomMedoid))
				if( ! data.isEmpty ) go(toTreat, medoids, clusterID + 1)
				else medoids	
			}
			else medoids
		}

		new RLAModel[O](go(data, mutable.HashMap.empty[Int, O], 0), metric)
	}
}
/**
 * Compagnion object to run the algorithm effortlessly
 */
object RLA {
	/**
	 * Run the RLA
	 */
	def run[O](data: GenSeq[O], epsilon: Double, metric: Distance[O]): RLAModel[O] = {
		val rlc = new RLA(data, epsilon, metric)
		val rlcModel = rlc.run()
		rlcModel
	}
}