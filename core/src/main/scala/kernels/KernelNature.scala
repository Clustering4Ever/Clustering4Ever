package clustering4ever.scala.kernels

object KernelNature extends Enumeration
{
    type KernelType = Value
    val Flat,
    	KNN,
    	Gaussian,
    	Sigmoid = Value
}