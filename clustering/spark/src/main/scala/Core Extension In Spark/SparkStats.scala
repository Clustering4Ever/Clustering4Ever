package clustering4ever.stats

import scala.reflect.ClassTag
import org.apache.spark.rdd.RDD
import org.apache.spark.storage.StorageLevel

object SparkStats {

	def obtainMinAndMax[S <: Seq[Double]](vectorizedDataset: RDD[S]) =
	{
		val dim = vectorizedDataset.first.size
		val vectorRange = (0 until dim).toBuffer
		val (minValues, maxValues) = vectorizedDataset.map{ v =>
			val vector = v.toBuffer
			(vector, vector)
		}.reduce( (minMaxa, minMaxb) => vectorRange.map( i => Stats.obtainIthMinMax(i, minMaxa, minMaxb) ).unzip )

		(minValues, maxValues)
	}
}