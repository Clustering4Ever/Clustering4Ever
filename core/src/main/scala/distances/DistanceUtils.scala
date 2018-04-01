package clustering4ever.math.distances

import _root_.clustering4ever.math.distances.binary.{Hamming, MeanMahanttan, PatternDifference, ShapeDifference, SizeDifference, Vari}
import _root_.clustering4ever.math.distances.scalar.{Cosine, Euclidean, Minkowski}

/**
 * @author Beck Gaël
 * DistanceUtils implements methods facilitating call for a specific metric
 **/
object DistanceUtils
{
	def chooseBinaryDistance(distanceAsStr: String): BinaryDistance =
	{
		val choosenDistance = distanceAsStr match
		{
	        case "Hamming" => Some(new Hamming)
	        case "MeanMahanttan" => Some(new MeanMahanttan)
	        case "PatternDifference" => Some(new PatternDifference)
	        case "ShapeDifference" => Some(new ShapeDifference)
	        case "SizeDifference" => Some(new SizeDifference)
	        case "Vari" => Some(new Vari) 
	        case _ =>
	        {
	        	throw new IllegalArgumentException("You choose a wrong Distance name")
    			None
    		}
    	}
		choosenDistance.get
	}

	def chooseScalarDistance(distanceAsStr: String): ContinuousDistances =
	{
		val splittedParam = distanceAsStr.split(":")
		val Array(distance, param) = if(splittedParam.size == 2) splittedParam else (splittedParam :+ "none")

		val choosenDistance = distance match
		{
	        case "Cosim" => Some(new Cosine)
	        case "Euclidean" => Some(new Euclidean(param.toBoolean))
	        case "Minkowski" => Some(new Minkowski(param.toInt))
	        case _ =>
	        {
	        	throw new IllegalArgumentException("You choose a wrong Distance name")
    			None
    		}
    	}
    	choosenDistance.get
   	}
}