package org.clustering4ever.preprocessing.umap

/**
 * @author Beugnet Vincent
 * @author Hurvois Guillaume
 * @author Ladjal Adlane
 * @author Merien Grégoire
 * @author Serfas Florent
 * @author Beck Gaël
 * @author Forest Florent
 */
import breeze.linalg._

import _root_.scala.collection.mutable
import _root_.scala.util.Random
/**
  * Representation of a Random Projection Split.
  * This projection can be defined either by an Euclidean or an Angular metric
  */
trait RPSplit extends Serializable {
    val data: Array[Array[Double]] // Data to be splitted
    val indices: mutable.ArrayBuffer[Int] // Indices to distribute on both sides of the space

    val rand = new Random
    final val dim: Int = data.head.length

    val leftIndices: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer[Int](0)
    val rightIndices: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer[Int](0)
    val hyperplane: DenseVector[Double] = new DenseVector(dim) // Hyperplane that halving the space

    def describe(): Unit = {
        println("Ind left :")
        leftIndices.foreach(println)

        println("Ind right :")
        rightIndices.foreach(println)

        println("Hyperplane Vector :")
        hyperplane.foreach(println)
    }
}


/**
  * Given a set of ``indices`` for data points from ``data``, create
  * a random hyperplane to split the data, returning two arrays indices
  * that fall on either side of the hyperplane. This is the basis for a
  * random projection tree, which simply uses this splitting recursively.
  * This particular split uses cosine distance to determine the hyperplane
  * and which side each data sample falls on.
  *
  * @param data    The original data to be split
  * @param indices The indices of the elements in the ``data`` array that are to
  *                be split in the current operation.
  */
// final case class AngularRPSplit(data: Array[Array[Double]], indices: mutable.ArrayBuffer[Int], rngState: Array[Long]) extends RPSplit {


//     split

//     def split {

//         val nbPoints = indices.length
//         val zeroUdim = 0 until dim

//         /* Selection of two random index to determine an hyperplane through this points*/
//         val indexL = rand.nextInt(Int.MaxValue) % nbPoints
//         var indexR = rand.nextInt(Int.MaxValue) % nbPoints
//         if (indexR == indexL) indexR = indexR + 1
//         indexR = Utils.mod(indexR, nbPoints)

//         val left = indices(indexL)
//         val right = indices(indexR)

//         val leftVec = data(left, zeroUdim).t
//         val rightVec = data(right, zeroUdim).t
//         val leftPreNorm = norm(leftVec)
//         val rightPreNorm = norm(rightVec)
//         val leftNorm = if (leftPreNorm == 0D) 1D else leftPreNorm
//         val rightNorm = if (rightPreNorm == 0D) 1D else rightPreNorm
        

//         /* Compute the normal vector to that hyperplane, to determine the hyperplane that halving the space */
//         zeroUdim.foreach( d => hyperplane(d) = data(left, d) / leftNorm - data(right, d) / rightNorm )

//         val hyperplanePreNorm = norm(hyperplane)
//         val hyperplaneNorm = if (hyperplanePreNorm == 0D) 1D else hyperplanePreNorm

//         zeroUdim.foreach( d => hyperplane(d) = hyperplane(d) / hyperplaneNorm )


//         /* Determine which part of the space each point belongs */
//         val zeroUnbPoints = 0 until nbPoints
//         val side = new Array[Boolean](nbPoints)
//         var nRight = 0
//         var nLeft = 0
//         for (i <- zeroUnbPoints) {
//             var margin = 0D
//             zeroUdim.foreach(d => margin = margin + hyperplane(d) * data(indices(i), d))

//             if (margin == 0) {
//                 side(i) = rand.nextBoolean()
//                 if (side(i)) nRight = nRight + 1
//                 else nLeft = nLeft + 1
//             }
//             else if (margin > 0) {
//                 side(i) = false
//                 nLeft = nLeft + 1
//             }
//             else {
//                 side(i) = true
//                 nRight = nRight + 1
//             }
//         }

//         for (i <- zeroUnbPoints) {
//             if (side(i)) rightIndices += indices(i)
//             else leftIndices += indices(i)
//         }
//     }

// }

/**
  * Given a set of ``indices`` for data points from ``data``, create
  * a random hyperplane to split the data, returning two arrays indices
  * that fall on either side of the hyperplane. This is the basis for a
  * random projection tree, which simply uses this splitting recursively.
  * This particular split uses euclidean distance to determine the hyperplane
  * and which side each data sample falls on.
  *
  * @param data    The original data to be split
  * @param indices The indices of the elements in the ``data`` array that are to
  *                be split in the current operation.
  */

final case class EuclideanRPSplit(val data: Array[Array[Double]], val indices: mutable.ArrayBuffer[Int], val rngState: Array[Long] = Array(2, 1, 1)) extends RPSplit {

    var offset: Double = 0D

    split

    private def split: Unit = {
        val nbPoints = indices.length
        val zeroUdim = 0 until dim

        /* Selection of two random index to determine the hyperplane through this points*/
        def mod(a: Int, b: Int) = {
            val res = a % b
            if (res < 0) res + b
            else res
        }

        val indexL = rand.nextInt(nbPoints)
        var indexR = rand.nextInt(nbPoints)

        if (indexR == indexL) indexR = indexR + 1
        indexR = indexR % nbPoints

        val left = indices(indexL)
        val right = indices(indexR)

        /* Compute the normal vector to that hyperplane, to determine the hyperplane that halving the space */
        zeroUdim.foreach( d => hyperplane(d) = data(left)(d) - data(right)(d) )
        zeroUdim.foreach( d => offset = offset - hyperplane(d) * (data(left)(d) + data(right)(d)) / 2 )

        /* Determine which part of the space each point belongs */
        val zeroUnbPoints = 0 until nbPoints
        val side = new Array[Boolean](nbPoints)
        var nRight = 0
        var nLeft = 0
        for (i <- zeroUnbPoints) {
            var margin = offset
            zeroUdim.foreach(d => margin = margin + hyperplane(d) * data(indices(i))(d))

            if (margin == 0) {
                val alea3 = (new Random).nextInt(2)
                side(i) = alea3 % 2 == 1
                if (side(i)) nRight = nRight + 1
                else nLeft = nLeft + 1
            } else if(margin > 0) {
                side(i) = false
                nLeft = nLeft + 1
            } else {
                side(i) = true
                nRight = nRight + 1
            }
        }

        for (i <- zeroUnbPoints) {
            if (side(i)) {
                rightIndices += indices(i)
            } else {
                leftIndices += indices(i)
            }
        }
    }

    override def describe(): Unit = {
        super.describe()
        println("Hyperplane offset")
        println(offset)
    }
}

/**
  * Given a set of ``indices`` for data points from ``data``, create
  * a random hyperplane to split the data, returning two arrays indices
  * that fall on either side of the hyperplane. This is the basis for a
  * random projection tree, which simply uses this splitting recursively.
  * This particular split uses euclidean distance to determine the hyperplane
  * and which side each data sample falls on.
  *
  * @param data    The original data to be split
  * @param indices The indices of the elements in the ``data`` array that are to
  *                be split in the current operation.
  */

// final case class EuclideanRPSplitReggg(data: DenseMatrix[Double] /* Instance[Array[Double]] */, indices: mutable.ArrayBuffer[Int], rngState: Array[Long] = Array(2, 1, 1)) extends RPSplit {

//     var offset: Double = 0D

//     split

//     private def split: Unit = {
//         val nbPoints = indices.length
//         val zeroUdim = 0 until dim

//         /* Selection of two random index to determine the hyperplane through this points*/
//         def mod(a: Int, b: Int) = {
//             val res = a % b
//             if (res < 0) res + b
//             else res
//         }
//         val alea1 = Utils.tauRandInt(rngState)
//         val alea2 = Utils.tauRandInt(rngState)
//         val indexL = mod(alea1, nbPoints)
//         var indexR = mod(alea2, nbPoints)
//         if (indexR == indexL) indexR = indexR + 1
//         indexR = indexR % nbPoints

//         val left = indices(indexL)
//         val right = indices(indexR)

//         /* Compute the normal vector to that hyperplane, to determine the hyperplane that halving the space */
//         zeroUdim.foreach( d => hyperplane(d) = data(left, d) - data(right, d) )
//         zeroUdim.foreach( d => offset = offset - hyperplane(d) * (data(left, d) + data(right, d)) / 2 )

//         /* Determine which part of the space each point belongs */
//         val zeroUnbPoints = 0 until nbPoints
//         val side = new Array[Boolean](nbPoints)
//         var nRight = 0
//         var nLeft = 0
//         for (i <- zeroUnbPoints) {
//             var margin = offset
//             zeroUdim.foreach(d => margin = margin + hyperplane(d) * data(indices(i), d))

//             if (margin == 0) {
//                 val alea3 = (new Random).nextInt(2)
//                 side(i) = alea3 % 2 == 1
//                 if (side(i)) nRight = nRight + 1
//                 else nLeft = nLeft + 1
//             } else if(margin > 0) {
//                 side(i) = false
//                 nLeft = nLeft + 1
//             } else {
//                 side(i) = true
//                 nRight = nRight + 1
//             }
//         }

//         for (i <- zeroUnbPoints) {
//             if (side(i)) {
//                 rightIndices += indices(i)
//             } else {
//                 leftIndices += indices(i)
//             }
//         }
//     }

//     override def describe(): Unit = {
//         super.describe()
//         println("Hyperplane offset")
//         println(offset)
//     }
// }
