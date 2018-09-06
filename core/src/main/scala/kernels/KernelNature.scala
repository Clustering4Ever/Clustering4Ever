package clustering4ever.scala.kernels

object KernelNature extends Enumeration {
    type KernelType = Value
    val Flat,
    	KNN,
    	EuclideanKNN,
    	Gaussian,
    	Sigmoid = Value
}