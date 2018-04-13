package clustering4ever.spark.clustering.clusterwise

import _root_.scala.util.Random
import breeze.linalg.DenseMatrix
import _root_.scala.collection.mutable.{ArrayBuffer, HashMap}
import _root_.scala.collection.immutable.IndexedSeq
import util.control.Breaks._

class ClusterwiseCore(
	val dsXYTrain: Array[(Int, (Array[Double],Array[Double]))],
	var h:Int,
	var g:Int)(
	var allGroupedData: Option[HashMap[Int,Int]],
	var nbBloc:Int,
	var nbMaxAttemps: Int
)  extends ClusterwiseTypes with Serializable
{	

	val rangeOverClasses = (0 until g).toArray

	def removeLastXY(_class: Int, inputX: IDXDS, inputY: YDS) =
	{
	  	inputX(_class).remove( inputX(_class).size - 1 )	
	  	inputY(_class).remove( inputY(_class).size - 1 )
	}

	def posInClassForMovingPoints(currClass: Int, elemNb: Int, limitsClass: Array[Int]) =
	{
		if( currClass == 0 ) elemNb else elemNb - limitsClass( currClass - 1 ) - 1
	}

	val removeFirstElemXY = (_class: Int, xDS: IDXDS, yDS: YDS) =>
	{
		xDS(_class).remove(0)
		yDS(_class).remove(0)
	}

	def prepareMovingPoint(classedDS: ClassedDS, xDS:  IDXDS, yDS: YDS, g: Int, elemNb: Int, currClass: Int, limitsClass: Array[Int]) =
	{
		val posInClass = posInClassForMovingPoints(currClass, elemNb, limitsClass)
		val (elemToReplace_ID, (elemToReplace_X, elemToReplace_Y, _)) = classedDS(currClass)._2(posInClass)
		for( j <- 0 until g )
		{
		  if( j == currClass ) removeFirstElemXY(j, xDS, yDS)
		  else
		  {
		    xDS(j) += ( (elemToReplace_ID, elemToReplace_X) )
		    yDS(j) += elemToReplace_Y
		  }
		}
	}

	def prepareMovingPointByGroup(classedDS: ClassedDSperGrp, xDS:IDXDS,  yDS:YDS, g:Int, elemNb:Int, currClass:Int, limitsClass:Array[Int], orderedBucketSize:Array[Int]) =
	{
		val posInClass = posInClassForMovingPoints(currClass, elemNb, limitsClass)
		val elemToReplace = classedDS(currClass)._2(posInClass)._3
		for( j <- 0 until g )
		{
		  if( j == currClass )
		  {
		    for( i <- 0 until orderedBucketSize(elemNb) )
		  	{
		    	removeFirstElemXY(j, xDS, yDS)
		  	}
		  }
		  else
		  {
		    xDS(j) ++= elemToReplace.map{ case(grpId, id, x, y) => (id, x) }
		    yDS(j) ++= elemToReplace.map{ case(grpId, id, x, y) => y }
		  }
		}
	}


	def elseCaseWhenComputingError(errorsIndexes: Array[((Double, Double), Int)], boolTab: Array[Boolean], currentClass: Int) =
	{
		var b = true
		errorsIndexes.map{ case ((err1, err2), idx) =>
		{
			if( idx == currentClass ) err2
			else
			{
  			if( boolTab(idx) && b )
  			{
  				boolTab(idx) = false
  				b = false
  				err2
  			}
  			else err1
			}
		}}					  			
	}


	def plsPerDot() =
	{
		var continue = true
		var cptAttemps = 0
		var classOfEachData = Array.empty[(Int, Int)]
		var dsPerClassF = Array.empty[DSPerClass]
		var regPerClassFinal = Array.empty[RegPerClass]
	  	val mapRegCrit = HashMap.empty[Int, Double]

		do
		{
			try
			{
				cptAttemps += 1
			  	// Set randomly a class to each data point
			  	val classedDS = dsXYTrain.map{ case (id, (x, y)) => (id, (x, y, Random.nextInt(g)))}.sortBy{ case (id, (x, y, _class)) => _class}
			  	var valuesToBrowse = classedDS.map{  case (id, (x, y, _class)) => (id, _class) }
			  	//val valuesToBrowseMap = HashMap(valuesToBrowse.map(_._1).zipWithIndex:_*)
			  	//val tmpError = ArrayBuffer.empty[IndexedSeq[(Int, Int, Double, Double)]]	
				var dsPerClass = classedDS.groupBy{ case (id, (x, y, _class)) => _class }.toArray.sortBy{ case (_class, idXYClass) => _class }
				//val classPosByID = HashMap.empty[Int, (Int, Int)]
			  	val inputX = dsPerClass.map{ case (_class, idXYClass) => ArrayBuffer(idXYClass.map{ case (id, (x, y, _class))  => (id, x) }:_*) }
			  	val inputY = dsPerClass.map{ case (_class, idXYClass) => ArrayBuffer(idXYClass.map{ case (id, (x, y, _class)) => y }:_*) }
			  	var preLimitsClass = (for( i <- 0 until inputY.size ) yield( inputY(i).size )).toArray
			  	var limitsClass = (for( i <- 0 until preLimitsClass.size ) yield( (for( j <- 0 to i ) yield( preLimitsClass(j) )).reduce(_ + _) )).map(_ - 1).toArray
			  	var currentDotIdx = 0
			  	var currentClass = 0
			  	var nbIte = 0
			  	val stop = valuesToBrowse.size - 1
			  	var numberBatch = 0

		  		breakable
		  		{
			  		if( inputX.size != g ) break
			  		if( inputX.exists(_.size == 0) ) break
				  	
				  	while( continue && nbIte != stop )
				  	{
					  	val (currentDotId, currentDotClass) = valuesToBrowse(nbIte)
					  	val regPerClass = for( i <- rangeOverClasses ) yield PLS.runPLS(inputX, inputY, i, h)
					  	// Temporary WorkAround when reduce data
					  	if( ! regPerClass.map(_._1).filter(_.isNaN).isEmpty ) break
					  	
					  	val error1 = regPerClass.map(_._1)
					  	prepareMovingPoint(dsPerClass, inputX, inputY, g, currentDotIdx, currentClass, limitsClass)
					  	val regPerClass2 =
				  		try
				  		{
				  			for( i <- rangeOverClasses ) yield PLS.runPLS(inputX, inputY, i, h)
				  		}
				  		catch
				  		{
				  			case emptyClass : java.lang.IndexOutOfBoundsException => Array.empty[RegPerClass]
				  		}
					  	
					  	if( regPerClass2.isEmpty ) break
					  	
					  	val error2 = regPerClass2.map(_._1)
					  	val boolTab = Array.fill(g)(true)
					  	val errorsIdx = error1.zip(error2).zipWithIndex
					  	//tmpError += errorsIdx.map{ case ((err1, err2), idx) => (currentDotId, idx, err1, err2) }
					  	boolTab(currentDotClass) = false
					  	val errors = for( i <- rangeOverClasses ) yield(
					  	{
					  		if( i == currentDotClass ) errorsIdx.map{ case ((err1, err2), idx) => err1 }
					  		else elseCaseWhenComputingError(errorsIdx, boolTab, currentDotClass)
					  	}).sum
					  	val minError = errors.min
					  	val classToMovePointInto = errors.indexOf(minError)
					  	val (point_ID, (point_X, point_Y, _)) = classedDS(currentDotIdx)
					  	if( classToMovePointInto != currentDotClass )
					  	{
						  	classedDS(currentDotIdx) = (point_ID, (point_X, point_Y, classToMovePointInto))
						  	val classWithoutDot = rangeOverClasses.filter( _class => _class != classToMovePointInto && _class != currentDotClass)
						  	for( j <- classWithoutDot ) removeLastXY(j, inputX, inputY)
					  	}
					  	else
					  	{
						  	val classWithoutDot = rangeOverClasses.filter(_ != currentDotClass)
						  	for( j <- classWithoutDot ) removeLastXY(j, inputX, inputY)
							inputX(currentDotClass) += ( (point_ID, point_X) )
							inputY(currentDotClass) += point_Y
					  	}
					  	continue = inputX.filter(_.isEmpty).isEmpty
					  	mapRegCrit += ( currentDotId -> minError )
					  	
					  	nbIte += 1
				  		
				  		currentDotIdx += 1
				  		if( currentDotIdx > limitsClass(currentClass) ) currentClass += 1
				  	}
			  	}
			  	continue = nbIte != stop
			  	if( continue ) mapRegCrit.clear
			  	else
			  	{
					dsPerClassF = for( i <- rangeOverClasses ) yield( classedDS.filter{ case (_, (_, _, _class)) => _class == i } )
					regPerClassFinal = for( i <- rangeOverClasses ) yield PLS.runPLS(inputX, inputY, i, h)
					classOfEachData = classedDS.map{ case (id, (_, _, _class)) => (id, _class) }	
			  	}
			}
			catch
			{ 
				case svdExcept : breeze.linalg.NotConvergedException =>
				{
					mapRegCrit.clear
					println("\nThere was an Singular Value Decomposition Issue, retry with new initialisation")
				}
			}
		}
		while( continue && cptAttemps < nbMaxAttemps )

		if( continue && cptAttemps == nbMaxAttemps ) throw new Exception("There was too many unsuccesufull attemps due to empty classes, try to diminish number of class ")
		

	  	val resReg = regPerClassFinal.map(_._1)
	  	val coXYcoef = regPerClassFinal.map(_._2.toArray)
	  	val coIntercept = regPerClassFinal.map(_._3.toArray)
	  	val pred = regPerClassFinal.map(_._4)	

	  	(dsPerClassF, pred, coIntercept, coXYcoef, resReg, mapRegCrit, classOfEachData)

	}

	def plsPerGroup() =
	{
		var continue = true
		var cptAttemps = 0
		var classOfEachData = Array.empty[(Int, Int)]
		var dsPerClassF = Array.empty[ClassedDSperGrp]
		var regPerClassFinal = Array.empty[RegPerClass]
	  	val mapRegCrit = HashMap.empty[Int,Double]	

	  	do
	  	{
			try
			{
		  		cptAttemps += 1
			  	// Initialisation par groupe
			  	val perGroupClassInit = for( i <- 0 until nbBloc ) yield(Random.nextInt(g))

			  	val dsPerClassPerBucket = dsXYTrain.map{ case (id, (x, y)) => (allGroupedData.get(id), id, x, y) }
					.groupBy(_._1)
					.toArray
					.map{ case (grpId, grpId_id_x_y) => (perGroupClassInit(grpId), grpId, grpId_id_x_y) }
					.groupBy{ case (_class, grpId, grpId_id_x_y) => _class }
					.toArray
					.sortBy{ case (_class, _) => _class }

				val bucketOrderPerClass = (for( (_class, buckets) <- dsPerClassPerBucket ) yield ((_class, buckets.map{ case (_class, grpId, grpId_id_x_y) => grpId }))).toArray

				val indexedFlatBucketOrder = bucketOrderPerClass.flatMap{ case (_class, grpIds) => grpIds.map( grpId => (grpId, _class) ) }.zipWithIndex

				val preSize = dsPerClassPerBucket.map{ case (_class, dsPerBucket) => dsPerBucket.map{ case (_class, grpId, ds) => ds.size } }
				val orderedBucketSize = preSize.flatten
				val classSize = preSize.map(_.size )
				val classSize2 = preSize.map(_.reduce(_ + _))
				val limitsClass = (for( i <- 0 until classSize.size ) yield( (for( j <- 0 to i ) yield( classSize(j) )).reduce(_ + _) )).map(_ - 1).toArray

			  	val inputX = dsPerClassPerBucket.map{ case (_class, dsPerBucket) => ArrayBuffer(dsPerBucket.flatMap{ case (_class, grpId, ds) => ds.map{ case (grpId, id, x, y) => (id, x) }}:_*)}
			  	val inputY = dsPerClassPerBucket.map{ case (_class, dsPerBucket) => ArrayBuffer(dsPerBucket.flatMap{ case (_class, grpId, ds) => ds.map{ case (grpId, id, x, y) => y }}:_*)}

			  	var currentDotsGrpIdx = 0
			  	var currentClass = 0
			  	var nbIte = 0
				val stop = indexedFlatBucketOrder.size - 1

		  		breakable
		  		{
			  		// if init starts with empty classes, retry
			  		if( inputX.size != g ) break
			  		if( inputX.exists(_.size == 0) ) break

				  	while( continue && nbIte != stop )
				  	{
				  		val ((grpId, current_class), current_idx) = indexedFlatBucketOrder(nbIte)
				  	
					  	// Regression with Point inside one Class and not the Rest
					  	val regPerClass = for( i <- rangeOverClasses ) yield PLS.runPLS(inputX, inputY, i, h)
					  	val error1 = regPerClass.map(_._1)
					  	prepareMovingPointByGroup(dsPerClassPerBucket, inputX, inputY, g, currentDotsGrpIdx, currentClass, limitsClass, orderedBucketSize)
					  	if( inputX.exists(_.size == 0) )
					  	{
					  		println("Problemo one class is becoming empty")
					  		break
					  	}
					  	// Regression with Point inside all other Class and not the former
					  	val regPerClass2 =
					  	{

					  		try for( i <- rangeOverClasses ) yield PLS.runPLS(inputX, inputY, i, h)
					  		catch { case emptyClass : java.lang.IndexOutOfBoundsException => Array.empty[RegPerClass] }
					  	}

					  	if( regPerClass2.isEmpty ) break
			  	  		val error2 = regPerClass2.map(_._1)
					  	val boolTab = Array.fill(g)(true)
					  	val errorsIdx = error1.zip(error2).zipWithIndex

					  	boolTab(current_class) = false
					  	val errors = for( i <- rangeOverClasses ) yield(
					  	{
					  		if( i == current_class ) errorsIdx.map{ case ((err1, err2), idx) => err1 }
					  		else elseCaseWhenComputingError(errorsIdx, boolTab, current_class)

					  	}).sum
					  	val minError = errors.min
					  	val classToMoveGroupInto = errors.indexOf(minError)
					  	val posInClass = posInClassForMovingPoints(currentClass, currentDotsGrpIdx, limitsClass)
					  	val (_, bucketIDtoUpdate, grpIdIDXY) = dsPerClassPerBucket(currentClass)._2(posInClass)
					  	if( classToMoveGroupInto != current_class )
					  	{
						  	dsPerClassPerBucket(currentClass)._2(posInClass) = (classToMoveGroupInto, bucketIDtoUpdate, grpIdIDXY)
						  	val classWithoutDots = rangeOverClasses.filter( _class => _class != classToMoveGroupInto && _class != current_class)
						  	for( j <- classWithoutDots )
						  	{
						  		for( i <- 0 until orderedBucketSize(currentDotsGrpIdx) )
						  		{
						  			removeLastXY(j, inputX, inputY)
						  		}
						  	}
					  	}
					  	else
					  	{
						  	val classWithoutDots = rangeOverClasses.filter(_ != current_class)
						  	for( j <- classWithoutDots )
						  		for( i <- 0 until orderedBucketSize(currentDotsGrpIdx) ) removeLastXY(j, inputX, inputY)
						  	inputX(current_class) ++= grpIdIDXY.map{ case (grpId, id, x, y) => (id, x) }
						  	inputY(current_class) ++= grpIdIDXY.map{ case (grpId, id, x, y) => y }
					  	}
					  	mapRegCrit += ( current_idx -> minError )
					  	continue = inputX.filter(_.isEmpty).isEmpty
						nbIte += 1
				  		currentDotsGrpIdx += 1
				  		if( currentDotsGrpIdx > limitsClass(currentClass) ) currentClass += 1
			  		}
			  	}
			  	continue = nbIte != stop
			  	if( continue ) mapRegCrit.clear
			  	else
			  	{
			  		dsPerClassF = for( i <- rangeOverClasses ) yield (dsPerClassPerBucket.filter{ case (_class, _) => _class == i })
					regPerClassFinal = for( i <- rangeOverClasses ) yield PLS.runPLS(inputX, inputY, i, h)
			  		classOfEachData = dsPerClassPerBucket.flatMap{ case (_class, dsPerBucket) => dsPerBucket.flatMap{ case (_, grpId, ds) => ds.map{ case (_, id, x, y) => (id,_class) } } }
			  	}
			}
			catch
			{
				case svdExcept : breeze.linalg.NotConvergedException =>
				{
					mapRegCrit.clear
					println("\nThere was an Singular Value Decomposition Issue, retry with new initialisation")
				}
			}
		}
		while( continue && cptAttemps < nbMaxAttemps )

		if( continue && cptAttemps == nbMaxAttemps ) throw new Exception("There was too many unsuccesufull attemps due to empty classes, try to diminish number of class or size of blocs")


	  	val resReg = regPerClassFinal.map(_._1)
	  	val coXYcoef = regPerClassFinal.map(_._2.toArray)
	  	val coIntercept = regPerClassFinal.map(_._3.toArray)
	  	val pred = regPerClassFinal.map(_._4)

	  	(dsPerClassF, pred, coIntercept, coXYcoef, resReg, mapRegCrit, classOfEachData)
	}
}
