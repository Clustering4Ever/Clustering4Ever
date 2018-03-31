package clustering4ever.math.distances

import _root.clustering4ever.math.distances.binary.{HammingDistance, MeanMahanttan, PatternDifference, ShapeDifference, SizeDifference, Vari}
import _root.clustering4ever.math.distances.scalar.{Cosine, Euclidean, Minkowski}
/**
 * @author Beck Gaël
 **/
object DistanceUtils
{
	def chooseBinaryDistance(distanceAsStr: String): BinaryDistance =
	{
		val choosenDistance = distanceAsStr match
		{
	        case "Hamming" => new HammingDistance
	        case "MeanMahanttan" => new MeanMahanttan
	        case "PatternDifference" => new PatternDifference
	        case "ShapeDifference" => new ShapeDifference
	        case "SizeDifference" => new SizeDifference
	        case "Vari" => new Vari 
	        case _ => { println("Default choice => Hamming"); new HammingDistance }
    	}
    	println(choosenDistance)
		choosenDistance
	}

	def chooseScalarDistance(distanceAsStr: String): ContinuousDistances =
	{
		val splittedParam = distanceAsStr.split(":")
		val Array(distance, param) = if(splittedParam.size == 2) splittedParam else (splittedParam :+ "none")

		val choosenDistance = distance match
		{
	        case "Cosim" => new Cosine
	        case "Euclidean" => new Euclidean(param.toBoolean)
	        case "Minkowski" => new Minkowski(param.toInt)
	        case _ => { println("Default choice => Euclidean"); new Euclidean(root = false) }
    	}
    	println(choosenDistance)
    	choosenDistance
   	}
}