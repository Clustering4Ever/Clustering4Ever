package clustering4ever.scala.basicenum
/**
 * @author Beck Gaël
 */
/**
 *
 */
abstract class AlternativeVector(val nature: String) extends Serializable
/**
 *
 */
object AlternativeVectorNature {

	case object Gradient_Ascent extends AlternativeVector("Gradient Ascent")
	
	case object PCIZED extends AlternativeVector("PCIZED")

}
