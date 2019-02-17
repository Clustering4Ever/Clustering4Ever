package org.clustering4ever.clustering.arguments
/**
 * @author Beck Gaël
 */
import org.clustering4ever.math.distances.Distance
import org.clustering4ever.vectors.GVector
import org.clustering4ever.kernels.{Kernel, KernelArgs}
/**
 *
 */
trait AlgorithmsArguments extends Serializable
/**
 *
 */
trait MinShiftArgs extends AlgorithmsArguments {
	/**
	 * The minimal shift under which points are considered stationary
	 */
	val minShift: Double
}
/**
 *
 */
trait MaxIterationsArgs extends AlgorithmsArguments {
	/**
	 * The maximum iterations number allowed for this algorithm
	 */
	 val maxIterations: Int
}
/**
 *
 */
trait KArgs extends AlgorithmsArguments {
	/**
	 * k value for various application such as KNN, number of clusters
	 */
	val k: Int
}
/**
 *
 */
trait MetricArgs[V <: GVector[V], D <: Distance[V]] extends AlgorithmsArguments {
	/**
	 * The metric used in this algorithm
	 */
	val metric: D
}
/**
 *
 */
trait KernelAsArgs[V <: GVector[V], KArgs <: KernelArgs, K <: Kernel[V, KArgs]] extends AlgorithmsArguments {
  /**
   * The kernel used in this algorithm
   */
  val kernel: K
}