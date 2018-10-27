package clustering4ever.math.distances.scalar
/**
 * @author Beck Gaël
 */
import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.util.SumVectors

class Cosine[V <: Seq[Double]] extends ContinuousDistance[V] {
	/**
	  * The cosine distance
	  * @return The cosine distance between dot1 and dot2
	  */
	override def d(dot1: V, dot2: V): Double = {
		val anorm = SumVectors.norm(dot1)
		val bnorm = SumVectors.norm(dot2)
		SumVectors.dotProd(dot1, dot2) / (anorm * bnorm)
	}
}





