package org.clustering4ever.util

/**
 * @author Beck Gaël
 */
object RecursivFunctions extends Serializable {
	/**
	 * TO DO use more specialized methods rather than f(X, X, T) => T to gain speed
	 */
	final def goOverMatrix[T](i: Int, j: Int, t: T, sizeColumn: Int, f: (Int, Int, T) => T): T = {
		@annotation.tailrec
		def go[T](i: Int, j: Int, t: T, f: (Int, Int, T) => T): T = {
		    if(i >= 0 && j > 0) go(i, j - 1, f(i, j, t), f)
		    else if(i > 0 && j == 0) go(i - 1, sizeColumn - 1, f(i, j, t), f)
		    else f(i, j, t)       
		}
		go(i, j, t, f)
	}
}