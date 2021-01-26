package org.clustering4ever.clustering.selforganizedmap.utils

/**
 * Created with IntelliJ IDEA.
 * User: tug
 * Date: 27/03/13
 * Time: 17:07
 * To change this template use File | Settings | File Templates.
 */
class NamedVector(val elements: Array[Double], val cls: Int) extends Serializable
{
  override def toString(): String = "#" + cls + " " + super.toString

  def toJSON(clusterId: Int): String =
  {
    var str = new StringBuilder
    str append "{"
    for ( i <- elements.indices)
    {
      str append "attr"+i+":"+elements(i)+", "
    }
    str append "cls:\""+cls+"\", "
    str append "clusterId:"+clusterId
    str append "}\n"
    str.toString
  }
}