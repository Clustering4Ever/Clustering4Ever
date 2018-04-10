package clustering4ever.spark.clustering.mtm.utils
//package org.lipn.som.utils

/**
 * Created with IntelliJ IDEA.
 * User: tug
 * Date: 27/03/13
 * Time: 19:10
 * To change this template use File | Settings | File Templates.
 */
class Matrix(val elements: Array[Array[Double]]) extends Serializable
{
  //def this(nbRow: Int, nbCol: Int) = this(Array.tabulate(nbRow, nbCol){case (r, c) => r+c})
  def this(nbElements: Int, initValue: Double) =
  {
    this(Array.fill(nbElements, nbElements)(initValue))
  }

  def fillRow(row: Int, value: Double) =
  {
    elements(row) = Array.fill(elements.length)(value)
  }
  
  def apply(row: Int, col: Int):Double =
  {
    elements(row)(col)
  }

  def addel(row: Int, col: Int, value: Double) =
  {
    elements(row)(col) += value
  }

  def += (other: Matrix): Matrix =
  {
    //todo: add length check
    //if (length != other.length)
    //  throw new IllegalArgumentException("Matrix of different length")
    var ans = 0D
    var i = 0
    for (i <- 0 until elements.length) {
      for (j <- 0 until elements(i).length) {
        elements(i)(j) += other.elements(i)(j)
      }
    }
    this
  }

  def /= (other: Matrix): Matrix = {
    //todo: add length check
    //if (length != other.length)
    //  throw new IllegalArgumentException("Matrix of different length")
    var ans = 0D
    var i = 0
    for (i <- 0 until elements.length)
    {
      for (j <- 0 until elements(i).length)
      {
        if (other.elements(i)(j) == 0)
        {
          elements(i)(j) /= Double.MinPositiveValue
        }
        else
        {
          elements(i)(j) /= other.elements(i)(j)
        }
      }
    }
    this
  }


  //override def toString = elements.mkString("|", " | ", "|")
  override def toString: String = {
    val str = new StringBuilder()
    for (row <- elements) {
      for (elem <- row) {
        str.append(" |").append("%.2f".format(elem))
      }
      str.append("|\n")
    //elements.foreach{r =>
      //str append row.mkString("|", " | ", "|")+"\n"
    }
    str.toString()
  }
}
