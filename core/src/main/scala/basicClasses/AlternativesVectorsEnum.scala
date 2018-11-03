package clustering4ever.scala.basicenum
/**
 * @author Beck Gaël
 */

object AlternativeVectorNature {

	trait AlternativeVector {
		val nature: String
	}
	case object Gradient_Ascent extends AlternativeVector { val nature = "Gradient Ascent"}
}
