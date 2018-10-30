package clustering4ever.math.distances.binary
/**
 * @author Beck Gaël
 */
import clustering4ever.math.distances.{BinaryDistance, BinaryDistanceUtil}
import scala.collection.mutable

class Vari extends BinaryDistance[mutable.ArrayBuffer] {

	def d(vector1: mutable.ArrayBuffer[Int], vector2: mutable.ArrayBuffer[Int]): Double = {
		val (a,b,c,d) = BinaryDistanceUtil.contingencyTable(vector1, vector2)
		(b + c).toDouble / (4 * (a + b + c + d))
	}
}