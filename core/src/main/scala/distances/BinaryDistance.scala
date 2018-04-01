package clustering4ever.math.distances

/**
 * @author Beck Gaël
 **/
trait BinaryDistance extends Distance[Array[Int]]
{
	def d(vector1: Array[Int], vector2: Array[Int]): Double
}