package clustering4ever.math.distances

/**
 * @author Beck Gaël
 **/
trait Distance extends Serializable
{
	type T
	def distance(vector1: Seq[T], vector2: Seq[T]): Double
}