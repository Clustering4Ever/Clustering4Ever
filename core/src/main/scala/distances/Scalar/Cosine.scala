package clustering4ever.math.distances.scalar

import clustering4ever.math.distances.ContinuousDistance
import clustering4ever.util.SumVectors
/**
 * @author Beck Gaël
 **/
class Cosine[S <: Seq[Double]] extends ContinuousDistance[S] {
	/**
	  * The cosine distance
	  * @return The cosine distance between dot1 and dot2
	  */
	override def d(dot1: S, dot2: S): Double = {
		val anorm = SumVectors.norm(dot1)
		val bnorm = SumVectors.norm(dot2)
		SumVectors.dotProd(dot1, dot2) / (anorm * bnorm)
	}
}





