
package interpreter

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

object key extends Enumeration {
    type key = Value
    val Major, Minor, Lydian, Eastern, Dorian, Messy, WholeSteps, Blues, PentatonicMajor, PentatonicMinor = Value
    var k : key = null
}


class interpreter extends Enumeration {
  import key._
  
  var k : key = null
  val r = new Random()
  
  val n : Int = 150  //n represents the number of characters that appear in the Huffman tree, to be interpreted
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
  
  def initializeInterpreter(x : Int) {
    if (x == 0) {  //"happy" sounding :)
      k = key.Major
    }
    if (x == 1) {  //"sad" sounding :(
      k = key.Minor
    }
    if (x == 2) {  //"eastern" sounding O.o
      k = key.Eastern
    }
    if (x == 3) {  //"bittersweet" sounding :S
      k = key.Lydian
    }
    if (x == 4) {  //"medieval" sounding --|===>       (<< sword)
      k = key.Dorian
    }
    if (x == 5) {  //"random" sounding ??
      k = key.Messy
    }
    if (x == 6) {  //"dream-like" sounding -_-
      k = key.WholeSteps
    }
    if (x == 7) {  //"bluesy" sounding XD
      k = key.Blues
    }
    if (x == 8) {  //"simple-happy" sounding :|)
      k = key.PentatonicMajor
    }
    if (x == 9) {  //"simple-sad" sounding :|(
      k = key.PentatonicMinor
    }
      Initialize
  }
  
  
  //Used in early code testing to make a distribution resembling characters in a code file
  def revert(num : Int, size : Int) : Int = {  //given a number from 0 to n^2-1, creates a number between 0 and n 
    var count = size
    var rev = 0
    while (num > count) {
      rev += 1
      count += size - rev
    }
    return rev
   
  }
  
  //Used in early code testing to make a distribution resembling characters in a code file
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
  
  //some redundancy in assignment so that it's easy to adjust if desired later
  def initializePitches() = {
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
    if (k == key.Eastern) {
      pitches(0) = 0
      pitches(1) = 3
      pitches(2) = 8
      pitches(3) = 1
      pitches(4) = 6
      pitches(5) = 12
      pitches(6) = 20
      pitches(7) = 15
      pitches(8) = 13
      pitches(9) = 18
      pitches(10) = 24
      pitches(11) = 27
      pitches(12) = 32
      pitches(13) = 25
      pitches(14) = 30
    }
    if (k == key.Lydian) {
      pitches(0) = 0
      pitches(1) = 7
      pitches(2) = 12
      pitches(3) = 4
      pitches(4) = 6
      pitches(5) = 2
      pitches(6) = 19
      pitches(7) = 9
      pitches(8) = 11
      pitches(9) = 16
      pitches(10) = 24
      pitches(11) = 14
      pitches(12) = 18
      pitches(13) = 21
      pitches(14) = 23
    }
    if (k == key.Dorian) {
      pitches(0) = 0
      pitches(1) = 7
      pitches(2) = 12
      pitches(3) = 3
      pitches(4) = 5
      pitches(5) = 2
      pitches(6) = 19
      pitches(7) = 9
      pitches(8) = 10
      pitches(9) = 15
      pitches(10) = 24
      pitches(11) = 14
      pitches(12) = 17
      pitches(13) = 21
      pitches(14) = 22
    }
    if (k == key.Messy) {
      pitches(0) = 0
      pitches(1) = 1
      pitches(2) = 2
      pitches(3) = 3
      pitches(4) = 4
      pitches(5) = 5
      pitches(6) = 6
      pitches(7) = 7
      pitches(8) = 8
      pitches(9) = 9
      pitches(10) = 10
      pitches(11) = 11
      pitches(12) = 12
      pitches(13) = 13
      pitches(14) = 14
    }
    if (k == key.WholeSteps) {
      pitches(0) = 0
      pitches(1) = 2
      pitches(2) = 4
      pitches(3) = 6
      pitches(4) = 8
      pitches(5) = 10
      pitches(6) = 12
      pitches(7) = 14
      pitches(8) = 16
      pitches(9) = 18
      pitches(10) = 20
      pitches(11) = 22
      pitches(12) = 24
      pitches(13) = 26
      pitches(14) = 28
    }
    if (k == key.Blues) {
      pitches(0) = 0
      pitches(1) = 7
      pitches(2) = 12
      pitches(3) = 3
      pitches(4) = 5
      pitches(5) = 6
      pitches(6) = 10
      pitches(7) = 19
      pitches(8) = 24
      pitches(9) = 15
      pitches(10) = 17
      pitches(11) = 18
      pitches(12) = 27
      pitches(13) = 22
      pitches(14) = 27
    }
    if (k == key.PentatonicMajor) {
      pitches(0) = 0
      pitches(1) = 2
      pitches(2) = 4
      pitches(3) = 7
      pitches(4) = 9
      pitches(5) = 12
      pitches(6) = 14
      pitches(7) = 16
      pitches(8) = 19
      pitches(9) = 21
      pitches(10) = 24
      pitches(11) = 26
      pitches(12) = 28
      pitches(13) = 31
      pitches(14) = 33
    }
    if (k == key.PentatonicMinor) {
      pitches(0) = 0
      pitches(1) = 3
      pitches(2) = 5
      pitches(3) = 7
      pitches(4) = 10
      pitches(5) = 12
      pitches(6) = 15
      pitches(7) = 17
      pitches(8) = 19
      pitches(9) = 22
      pitches(10) = 24
      pitches(11) = 27
      pitches(12) = 29
      pitches(13) = 31
      pitches(14) = 34
    }
  }
  
  //calculates number of notes to be used for a given character frequency (which is value)
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
  
  //value indicates which and how many notes will be used, based on pitches, intervals, triples, and quadruples of given interpreter
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
  
  //handles character values with up to 150 unique pitches and durations
  def getNotes(value : Int) : (Int, ArrayBuffer[Int]) = {
    var notes = getNote(value%30)
    var noteLength = (Math.pow(.5, ((value + 1)/30).toInt)).toInt
    if (noteLength == 0) {
      noteLength = 1
    }
    return (noteLength, notes)
  }

  
  //since the intervals, triples, and quadruples map between pitches that exist and not to new ones, we don't need different initialization for each key
  def initializeIntervals() {
      intervals(0) = (0,7)
      intervals(1) = (0,12)
      intervals(2) = (0,4)
      intervals(3) = (7,12)
      intervals(4) = (4,7)
      intervals(5) = (4,12)
      intervals(6) = (2,7)
      intervals(7) = (4,11)
  }
  
  
  def initializeTriples() {
      triples(0) = (0,4,7)
      triples(1) = (0,5,9)
      triples(2) = (2,7,11)
      triples(3) = (7,9,12)
      triples(4) = (16,21,24)
  }
  
   def initializeQuadruples() {
      quadruples(0) = (0,4,7,11)
      quadruples(1) = (0,5,9,14)
  }
  
  def Initialize() {
    initializePitches
    initializeIntervals
    initializeTriples
    initializeQuadruples
  }
  
}