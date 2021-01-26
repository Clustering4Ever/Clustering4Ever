package org.clustering4ever.clustering.meanshift

/**
 * @author Tarn Duong
 * @author Beck Gaël
 */
import breeze.numerics._

import scala.math.Pi
/**
 * Optimal Choice of NNMS <=> knn-Mean-Shift
 */
object OptimalKChoice {
	/**
	 * Compute the hyper volume of the unit ball of dension d
	 */
	final def unitHyperBallVolume(d: Int) = {
		pow(Pi, d / 2D) / exp(lgamma((d / 2D) + 1D))
	}
	/**
	 * Compute the hyper volume of the ball of rardius r in dension d
	 */
	final def hyperBallVolume(d: Int, r: Double) = {
		pow(Pi, d / 2D) * pow(r, d) / exp(lgamma((d / 2D) + 1D))
	}
	/**
	 * cf paper
	 * @param n dataset size
	 */
	final def optimalKchoice(n: Int, d: Int) = {
		unitHyperBallVolume(d) * pow((4D / (d + 4D)), (d / (d + 6D))) * pow(n, (6D / (d + 6D)))
	}
	/**
	 * @param n dataset size
	 * @param d dataset dimensions
	 * @param r ...
	 */	
	final def optimalKchoice(n: Int, d: Int, r: Double = 1D) = {
		hyperBallVolume(d, r) * pow((4D / (d + 2D + 2D * r)), (d / (d + 4D + 2D * r))) * pow(n, ((2D * r + 4D) / (d + 2D * r + 4D)))
	}
}