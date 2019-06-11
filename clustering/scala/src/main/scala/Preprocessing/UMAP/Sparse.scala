package org.clustering4ever.scala.umap
/**
 * @author Beugnet Vincent
 * @author Hurvois Guillaume
 * @author Ladjal Adlane
 * @author Merien Grégoire
 * @author Serfas Florent
 * @author Beck Gaël
 * @author Forest Florent
 */
import breeze.linalg.{DenseMatrix, max}
import breeze.numerics.pow
import scala.collection.mutable
/**
 *
 */
object Sparse extends Serializable {
    /**
      * Retrieves in an array all non-zero values from a matrix
      * @param m
      * @return Array with all non-zero values from m
      */
    def nonZeroValues(m: DenseMatrix[Double]): mutable.ArrayBuffer[Double] = {
        @annotation.tailrec
        def go(i: Int, j: Int, nzv: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = {
            if (i < m.rows) {
                if (j < m.cols) {
                    m(i, j) match {
                        case 0D => go(i, j + 1, nzv)
                        case x => go(i, j + 1, nzv += x)
                    }
                } else go(i + 1, 0, nzv)
            } else nzv
        }
        go(0, 0, new mutable.ArrayBuffer[Double])
    }

    /**
    * Retrieves in an array all non-zero values from a matrix
    * @param m
      * @return Array with all non-zero values from m
        */
    def nonZeroRows(m: DenseMatrix[Double]): mutable.ArrayBuffer[Int] = {
        @annotation.tailrec
        def go(i: Int, j: Int, nzv: mutable.ArrayBuffer[Int]): mutable.ArrayBuffer[Int] = {
            if (i < m.rows) {
                if (j < m.cols) {
                    m(i, j) match {
                        case 0d => go(i, j + 1, nzv)
                        case _ => go(i, j + 1, nzv += i)
                    }
                } else go(i + 1, 0, nzv)
            } else nzv
        }
        go(0, 0, new mutable.ArrayBuffer[Int])
    }

    /**
    * Retrieves in an array all non-zero values from a matrix
    * @param m
        * @return Array with all non-zero values from m
        */
    def nonZeroCols(m: DenseMatrix[Double]): mutable.ArrayBuffer[Int] = {
        @annotation.tailrec
        def go(i: Int, j: Int, nzv: mutable.ArrayBuffer[Int]): mutable.ArrayBuffer[Int] = {
            if (i < m.rows) {
                if (j < m.cols) {
                    m(i, j) match {
                        case 0d => go(i, j + 1, nzv)
                        case _ => go(i, j + 1, nzv += j)
                    }
                } else go(i + 1, 0, nzv)
            } else nzv
        }
        go(0, 0, new mutable.ArrayBuffer[Int])
    }


    def generalSSetIntersection(mat1: DenseMatrix[Double], mat2: DenseMatrix[Double], res: DenseMatrix[Double], mixWeight: Double = 0.5): DenseMatrix[Double] = {
        val data1 = nonZeroValues(mat1)
        val data2 = nonZeroValues(mat2)
        val leftMin = max(data1.min / 2.0, 1.0e-8)
        val rightMin = max(data2.min / 2.0, 1.0e-8)

        for (i <- 0 until res.rows) {
            for (j <- 0 until res.cols) {
                val a: Double = res(i, j) match {
                    case 0d => res(i, j)
                    case _ =>
                        val leftVal = mat1(i, j) match {
                            case 0d => leftMin
                            case _ => mat1(i, j)
                        }

                        val rightVal = mat2(i, j) match {
                            case 0d => rightMin
                            case _ => mat2(i, j)
                        }

                        if (leftVal > leftMin || rightVal > rightMin) {
                            if (mixWeight < 0.5) {
                                leftVal * pow(rightVal, mixWeight / (1.0 - mixWeight))
                            }
                            else {
                                pow(leftVal, (1.0 - mixWeight) / mixWeight) * rightVal
                            }
                        } else res(i, j)
                }
                res(i, j) = a
            }
        }
        res
    }

}
