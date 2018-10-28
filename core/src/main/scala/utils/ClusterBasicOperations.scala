package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
import clustering4ever.math.distances.Distance
import scala.language.higherKinds
/**
 *
 */
object ClusterBasicOperations {
	/**
	 * @return the medoid which minimize its distance from all others cluster members for any space
	 */
	def obtainMedoid[O](cluster: GenSeq[O], metric: Distance[O]): O = cluster.minBy( v1 => cluster.map( v2 => metric.d(v1, v2) ).sum )
	/**
	 * @return the medoid which minimize its distance from all others cluster members for real or binary space
	 */
	def obtainMedoid[@specialized(Int, Double) N: SNumeric, V[N] <: Seq[N], D <: Distance[V[N]]](cluster: GenSeq[V[N]], metric: D): V[N] = obtainMedoid(cluster, metric)
	/**
	 * @return the centroid of the given cluster composed by real vectors
	 * 
	 * It has sense only with Euclidean distance
	 */
	def obtainMean[V[Double] <: Seq[Double]](cluster: GenSeq[V[Double]]): V[Double] = SumVectors.sumColumnMatrix(cluster).map(_ / cluster.size).asInstanceOf[V[Double]]
	/**
	 * @return the centroid of the given cluster composed by binary vectors
	 * 
	 * It has sense only with Hamming distance
	 */
	def obtainMode[V[Int] <: Seq[Int]](cluster: GenSeq[V[Int]]): V[Int] = SumVectors.sumColumnMatrix(cluster).map( v => if( 2 * v >= cluster.size ) 1 else 0 ).asInstanceOf[V[Int]]
}