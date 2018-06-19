package clustering4ever.math.distances

/**
 * @author Beck Gaël
 **/
trait BinaryDistance extends Distance[Vector[Int]]
{
	def d(vector1: Vector[Int], vector2: Vector[Int]): Double
}