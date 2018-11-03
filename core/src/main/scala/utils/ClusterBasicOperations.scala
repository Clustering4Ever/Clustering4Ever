package clustering4ever.util
/**
 * @author Beck Gaël
 */
import scala.language.higherKinds
import scala.collection.GenSeq
import spire.math.{Numeric => SNumeric}
import clustering4ever.math.distances.{Distance, ContinuousDistance, BinaryDistance}
import clustering4ever.math.distances.binary.Hamming
import clustering4ever.math.distances.scalar.Euclidean
import clustering4ever.math.distances.mixt.HammingAndEuclidean
import clustering4ever.scala.measurableclass.BinaryScalarVector
import clustering4ever.util.VectorsBasicOperationsImplicits._
/**
 * Type Class is probably the solution to this casting meli melo, but it is hard to apply in such complex case without any usefull ressources
 */
object ClusterBasicOperations {
	/**
	 * @return the center which minimize its distance from all others cluster members for any space
	 * When it is euclidean or hamming distance which is used, the linear way to compute the center is applied, aka the mean and mode (majority vote)
	 * Tried to avoid cast with typeClass Medoid[O, D <: [O]] but didn't succeed to implement a generic form with Objects extending Medoid[...]
	 * Many attemps with defining type V <: Seq[Double] but implicit calls are refused
	 * Someone to render it more proper would be cool :)
	 */
	def obtainCenter[O, D <: Distance[O]](cluster: GenSeq[O], metric: D): O = {
	    metric match {
	      case euclidean if( euclidean.isInstanceOf[Euclidean[_]] ) => obtainMean(cluster.asInstanceOf[GenSeq[Seq[Double]]]).asInstanceOf[O]
	      case hamming if( hamming.isInstanceOf[Hamming[_]] ) => obtainMode(cluster.asInstanceOf[GenSeq[Seq[Int]]]).asInstanceOf[O]
	      case hammingAndEuclidean if( hammingAndEuclidean.isInstanceOf[HammingAndEuclidean[_, _]] ) => obtainMixtCenter(cluster.asInstanceOf[GenSeq[BinaryScalarVector[Seq[Int], Seq[Double]]]]).asInstanceOf[O]
	      // Look for point which minimize its distance to all others points
	      case _ => cluster.minBy{ v1 =>
				var sum = 0D
				cluster.foreach( altVectors => sum += metric.d(v1, altVectors) )
				sum
			}
	    }
	}
	/**
	 *
	 */
	private def transformPreMeanAndCastIt[V[Double] <: Seq[Double]](preMean: V[Double], clusterSize: Int) = preMean.map(_ / clusterSize).asInstanceOf[V[Double]]
	/**
	 * @return the centroid of the given cluster composed by real vectors
	 * 
	 * It has sense only with Euclidean distance
	 */
	def obtainMean[V[Double] <: Seq[Double]](cluster: GenSeq[V[Double]]): V[Double] = transformPreMeanAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 *
	 */
	private def transformPreModeAndCastIt[V[Int] <: Seq[Int]](preMode: V[Int], clusterSize: Int) = preMode.map( v => if( 2 * v >= clusterSize ) 1 else 0 ).asInstanceOf[V[Int]]
	/**
	 * @return the centroid of the given cluster composed by binary vectors
	 * 
	 * It has sense only with Hamming distance
	 */
	def obtainMode[V[Int] <: Seq[Int]](cluster: GenSeq[V[Int]]): V[Int] = transformPreModeAndCastIt(SumVectors.sumColumnMatrix(cluster), cluster.size)
	/**
	 *
	 */
	 def obtainMixtCenter[Vb[Int] <: Seq[Int], Vs[Double] <: Seq[Double]](cluster: GenSeq[BinaryScalarVector[Vb[Int], Vs[Double]]]): BinaryScalarVector[Vb[Int], Vs[Double]] = {
	 	val mixtVector: BinaryScalarVector[Vb[Int], Vs[Double]] = SumVectors.sumColumnMatrix(cluster)
	 	val binaryPart = transformPreModeAndCastIt(mixtVector.binary, cluster.size)
	 	val realPart = transformPreMeanAndCastIt(mixtVector.scalar, cluster.size)
	 	new BinaryScalarVector(binaryPart, realPart)
	}
}