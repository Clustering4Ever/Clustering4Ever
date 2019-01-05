package org.clustering4ever.enums
/**
 * @author Beck Gaël
 */
/**
 *
 */
object ClusteringAlgorithmEnum extends Enumeration {

	type ClusteringAlgorithmEnum = Value
    val KMeans,
    	KModes,
    	KPrototypes,
    	RLA = Value

}
/**
 *
 */
object InternalsIndexes extends Enumeration {
    type InternalsIndexesType = Value
    val DaviesBouldin,
        BallHall,
        Silhouette = Value
}
/**
 *
 */
object ExternalsIndexes extends Enumeration {
    type ExternalsIndexesType = Value
    val MI,
        NMI_Sqrt,
        NMI_Max = Value
}
/**
 *
 */
object NmiNormalizationNature extends Enumeration {
    type Normalization = Value
    val SQRT,
    	MAX = Value
}
/**
 * ENUM of different kernel types
 */
object KernelNature extends Enumeration {
    type KernelType = Value
    val Flat,
        KNN,
        KNN_Real,
        KNN_Euclidean,
        KNN_Hamming,
        Gaussian,
        Sigmoid = Value
}