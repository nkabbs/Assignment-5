
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
  }
  
  def revert(num : Int, range : Int) : Int = {
    var i = 1
    var count = 0
    var retVal : Int = -1
    while (retVal == -1) {
      if (num < i + count) {
        retVal = num - count
      }
      count += i
      i += 1
    }
    return i
  }
  
  def weightDistributor(size: Int, numNotes: Int): ArrayBuffer[Int] = {  //test size = 26, numNotes = 50... 
    var b = new ArrayBuffer[Int]()
    var i = 0
    var range : Int = (size + 1 + size%2) * (size / 2)// basically equals (size^2)/2... why?
    print(range)
    var value = 0
    for (i <- 0 to numNotes - 1) {
      value = revert(r.nextInt(range), range) - 2
      println(value)
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
    var notes = getNote(value)
    //duration is correlated to the frequency the note appears. 
    var duration = n / (value + (n/numDurations - (n/numDurations) % value)) 
    if (duration == 0) {
      duration = 1
    }
    return (duration, notes)
  }

  def initializeIntervals() {
    if (k == key.Major) {
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
    if (k == key.Major) {
      triples(0) = (0,4,7)
      triples(1) = (0,5,9)
      triples(2) = (2,7,11)
      triples(3) = (7,9,12)
      triples(4) = (16,21,24)
    }
  }
  
   def initializeQuadruples() {
    if (k == key.Major) {
      quadruples(0) = (0,4,7,11)
      quadruples(1) = (0,5,9,14)
    }
  }
  
  def InitializeMajor() {
    initializePitches
    initializeIntervals
    initializeTriples
    initializeQuadruples
    /*
    inter(0) = (0,4)
    inter(1) = (7,4)
    inter(2) = (12,4)
    inter(3) = (19,4)
    inter(4) = (5,4)
    inter(5) = (4,4)
    inter(6) = (2,4)
    inter(7) = (9,4)
    inter(8) = (11,4)
    inter(9) = (0,2)
    inter(10) = (7,2)
    inter(11) = (12,2)
    inter(12) = (19,2)
    inter(13) = (5,2)
    inter(14) = (4,2)
    inter(15) = (2,2)
    inter(16) = (9,2)
    inter(17) = (11,2)
    inter(18) = (0,1)
    inter(19) = (7,1)
    inter(20) = (12,1)
    inter(21) = (19,1)
    inter(22) = (5,1)
    inter(23) = (4,1)
    inter(24) = (2,1)
    inter(25) = (9,1)
    * 
    */
  }
  
}