package org.clustering4ever.clustering.selforganizedmap.utils

import org.apache.spark.rdd.RDD
import org.clustering4ever.clustering.selforganizedmap.global.AbstractModel

import java.io._

object WriterClusters{
  def js(data: RDD[NamedVector], model: AbstractModel, path: String) = {
    val writer = new PrintWriter(new File(path))

    val dataArray = data.collect
    var str = "var dataset = ["

    dataArray.foreach{ d =>
      val closestNeuron = model.findClosestPrototype(d.elements)
      if (d != dataArray.head) str += ','
      str += d.toJSON(closestNeuron.id)
    }

    str += "];"
    writer.write(str)
    writer.close()
  }
}
