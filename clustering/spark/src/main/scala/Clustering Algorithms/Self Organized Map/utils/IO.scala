package org.clustering4ever.spark.clustering.mtm.utils

import java.io.File

/**
 * Company : Altic - LIPN
 * User: Tugdual Sarazin
 * Date: 06/06/14
 * Time: 11:48
 */
object IO {

  /** Deletes each file or directory (recursively) in `files`.*/
  def delete(files: Iterable[File]): Unit = files.foreach(delete)

  /** Deletes `file`, recursively if it is a directory. */
  def delete(file: File)
  {
      val deleted = file.delete()
      if(!deleted && file.isDirectory)
      {
        delete(listFiles(file))
        file.delete
      }
  }

  /** Returns the children of directory `dir` that match `filter` in a non-null array.*/
  def listFiles(filter: java.io.FileFilter)(dir: File): Array[File] = wrapNull(dir.listFiles(filter))

  /** Returns the children of directory `dir` that match `filter` in a non-null array.*/
  def listFiles(dir: File, filter: java.io.FileFilter): Array[File] = wrapNull(dir.listFiles(filter))

  /** Returns the children of directory `dir` in a non-null array.*/
  def listFiles(dir: File): Array[File] = wrapNull(dir.listFiles())

  private def wrapNull(a: Array[File]) = if( ! Option(a).isDefined ) new Array[File](0) else a
}
