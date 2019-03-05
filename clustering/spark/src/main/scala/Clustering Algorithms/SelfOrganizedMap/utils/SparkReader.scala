package org.clustering4ever.spark.clustering.mtm


import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

/**
 * Company : Altic - LIPN
 * User: Tugdual Sarazin
 * Date: 07/01/14
 * Time: 12:37
 */
object SparkReader {
  def parse(sc: SparkContext, filePath: String, splitRegex: String): RDD[NamedVector] = {
    sc.textFile(filePath).map{ line =>
		val arrayDouble = line.split(splitRegex).map(_.toDouble)
		new NamedVector(arrayDouble.dropRight(1), arrayDouble.last.toInt)
    }
  }
}