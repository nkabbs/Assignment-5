
package interpreter

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object key extends Enumeration {
    type key = Value
    val Major, Minor, Lydian = Value
    var k : key = null
}


class interpreter extends Enumeration {
  import key._
  
  var k : key = null
  val r = new Random()
  
  val n : Int = 150
  val numDurations = 5
  val numPitches = 15
  val numIntervals = 8
  val numTriples = 5
  val numQuadruples = 2
  
  val pitches = new Array[Int](numPitches)
  val durations = new Array[Int](numDurations)
  val intervals = new Array[(Int,Int)](numIntervals)
  val triples = new Array[(Int,Int,Int)](numTriples)
  val quadruples = new Array[(Int,Int,Int,Int)](numQuadruples)
  val inter = new Array[(Int,Int)](n)
  
  def initializeInterpreter(x : key) {
    k = x
    if (x.equals(Major)) {
      InitializeMajor
    }
    if (x.equals(Minor)) {
      InitializeMajor   //obviously redundant, but will 
    }
    if (x.equals(Lydian)) {
      InitializeMajor   //obviously redundant, 
    }
  }
  
  def revert(num : Int, size : Int) : Int = {  //given a number from 0 to n^2-1, creates a number between 0 and n 
    var count = size
    var rev = 0
    while (num > count) {
      rev += 1
      count += size - rev
    }
    return rev
    /*var i = 1
    var count = 0
    var retVal : Int = -1
    
    while (retVal == -1) {
      if (num < i + count) {
        retVal = num - count
        i -= 1
      }
      count += i
      i += 1
    }
    return i
    * */
   
  }
  
  def weightDistributor(size: Int, numNotes: Int): ArrayBuffer[Int] = {  //test size = 26, numNotes = 50... 
    var b = new ArrayBuffer[Int]()
    var i = 0
    var range : Int = (size + 1 + size%2) * (size / 2)// basically equals (size^2)/2... why?
    var value = 0
    for (i <- 0 to numNotes - 1) {
      value = revert(r.nextInt(range), size) - 2
      b.append(value)
    }
    return b
  }
  
  def initializePitches() {
    if (k == key.Major) {
      pitches(0) = 0
      pitches(1) = 7
      pitches(2) = 12
      pitches(3) = 4
      pitches(4) = 5
      pitches(5) = 2
      pitches(6) = 19
      pitches(7) = 9
      pitches(8) = 11
      pitches(9) = 16
      pitches(10) = 24
      pitches(11) = 14
      pitches(12) = 17
      pitches(13) = 21
      pitches(14) = 23
    }
    if (k == key.Minor) {
      pitches(0) = 0
      pitches(1) = 7
      pitches(2) = 12
      pitches(3) = 3
      pitches(4) = 5
      pitches(5) = 2
      pitches(6) = 19
      pitches(7) = 8
      pitches(8) = 10
      pitches(9) = 15
      pitches(10) = 24
      pitches(11) = 14
      pitches(12) = 17
      pitches(13) = 20
      pitches(14) = 22
    }
    if (k == key.Lydian) {
      pitches(0) = 0
      pitches(1) = 7
      pitches(2) = 12
      pitches(3) = 3
      pitches(4) = 6
      pitches(5) = 2
      pitches(6) = 19
      pitches(7) = 8
      pitches(8) = 10
      pitches(9) = 15
      pitches(10) = 24
      pitches(11) = 14
      pitches(12) = 18
      pitches(13) = 20
      pitches(14) = 22
    }
  }
  
  def getNumNotes(value : Int) : Int = {
    if (value < numPitches) {
      return 1
    }
    else if (value < numPitches + numIntervals) {
      return 2
    }
    else if (value < numPitches + numIntervals + numTriples) {
      return 3
    }
    else if (value < numPitches + numIntervals + numTriples + numQuadruples) {
      return 4
    } else {
      return -1
    }
  }
  
  def getNote(value: Int) : ArrayBuffer[Int] = {
    val a = new ArrayBuffer[Int]
    if (value < numPitches) {
      a.append(pitches(value))
    } else if (value < numPitches + numIntervals) {
      a.append(intervals(value-numPitches)._1)
      a.append(intervals(value-numPitches)._2)
    } else if (value < numPitches + numIntervals + numTriples) {
      a.append(triples(value-numPitches-numIntervals)._1)
      a.append(triples(value-numPitches-numIntervals)._2)
      a.append(triples(value-numPitches-numIntervals)._3)
    } else {
      a.append(quadruples(value-numPitches-numIntervals-numTriples)._1)
      a.append(quadruples(value-numPitches-numIntervals-numTriples)._2)
      a.append(quadruples(value-numPitches-numIntervals-numTriples)._3)
      a.append(quadruples(value-numPitches-numIntervals-numTriples)._4)
    }
    return a
  }
  
  def getNotes(value : Int) : (Int, ArrayBuffer[Int]) = {
    var notes = getNote(value/numDurations)
    var duration = (Math.pow(.5, ((value + 1)/30).toInt) * 16).toInt
    println(value/numDurations)
    if (duration == 0) {
      duration = 1
    }
    return (duration, notes)
  }

  def initializeIntervals() {
    if (k == key.Major || k == key.Minor || k == key.Lydian) {
      intervals(0) = (0,7)
      intervals(1) = (0,12)
      intervals(2) = (0,4)
      intervals(3) = (7,12)
      intervals(4) = (4,7)
      intervals(5) = (4,12)
      intervals(6) = (2,7)
      intervals(7) = (4,11)
    }
  }
  
  
  def initializeTriples() {
    if (k == key.Major || k == key.Minor || k == key.Lydian) {
      triples(0) = (0,4,7)
      triples(1) = (0,5,9)
      triples(2) = (2,7,11)
      triples(3) = (7,9,12)
      triples(4) = (16,21,24)
    }
  }
  
   def initializeQuadruples() {
    if (k == key.Major || k == key.Minor || k == key.Lydian) {
      quadruples(0) = (0,4,7,11)
      quadruples(1) = (0,5,9,14)
    }
  }
  
  def InitializeMajor() {  //obviously these three methods are redundant, but it'll be nice to be able to mess with them later
    initializePitches
    initializeIntervals
    initializeTriples
    initializeQuadruples
  }
  
  def InitializeMinor() {
    initializePitches
    initializeIntervals
    initializeTriples
    initializeQuadruples
  }
  
  def InitializeLydian() {
    initializePitches
    initializeIntervals
    initializeTriples
    initializeQuadruples
  }
  
}