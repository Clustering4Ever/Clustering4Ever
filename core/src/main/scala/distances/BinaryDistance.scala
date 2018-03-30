package clustering4ever.math.distances

/**
 * @author Beck Gaël
 **/
trait BinaryDistance extends Distance
{
	type T = Int
	def distance(vector1: Seq[T], vector2: Seq[T]): Double
}