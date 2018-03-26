package lipn.spartakus.core.math.distances

trait Distance extends Serializable
{
	type T
	def distance(vector1: Seq[T], vector2: Seq[T]): Double
}