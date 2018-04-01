package clustering4ever.math.distances

/**
 * @author Beck Gaël
 * Most general notion of Distance, taking two object of type T and returning a Double
 **/
trait Distance[T] extends Serializable
{
	def d(obj1: T, obj2: T): Double
}