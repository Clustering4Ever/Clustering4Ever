package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
import clustering4ever.math.distances.Distance

object ClusterBasicOperations {
	/**
	 * @return the medoid which minimize its distance from all others cluster members for any space
	 */
	def obtainMedoid[O](cluster: GenSeq[O], metric: Distance[O]): O = cluster.minBy( v1 => cluster.map( v2 => metric.d(v1, v2) ).sum )
	/**
	 * @return the medoid which minimize its distance from all others cluster members for any space real or binary space
	 */
	def obtainMedoid[@specialized(Int, Double) N: SNumeric, S <: Seq[N], D <: Distance[S]](cluster: GenSeq[S], metric: D): S = obtainMedoid[S](cluster, metric)
	/**
	 * @return the centroid of the given cluster composed by real vectors
	 * 
	 * It has sense only with Euclidean distance
	 */
	def obtainMean[S <: Seq[Double]](cluster: GenSeq[S]): S = SumVectors.sumColumnMatrix[Double, S](cluster).map(_ / cluster.size).asInstanceOf[S]
	/**
	 * @return the centroid of the given cluster composed by binary vectors
	 * 
	 * It has sense only with Hamming distance
	 */
	def obtainMode[S <: Seq[Int]](cluster: GenSeq[S]): S = SumVectors.sumColumnMatrix[Int, S](cluster).map( v => if( 2 * v >= cluster.size ) 1 else 0 ).asInstanceOf[S]
}