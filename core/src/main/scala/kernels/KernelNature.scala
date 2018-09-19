package clustering4ever.scala.kernels
/**
 * @author Beck Gaël
 */
object KernelNature extends Enumeration {
    type KernelType = Value
    val Flat,
    	KNN,
    	EuclideanKNN,
    	Gaussian,
    	Sigmoid = Value
}