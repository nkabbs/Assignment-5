package backgroundmusicDSL

import java.io.{BufferedOutputStream, File, FileOutputStream}
import javax.sound.midi._

import interpreter.{interpreter, key}

import scala.collection.mutable.ArrayBuffer
import scala.language.{dynamics, implicitConversions}
import scala.util.Random
import collection.mutable.HashMap

class backgroundmusicDSL {
  val random = new Random

  var numSequences = 10
  val numInterpreters = 5

  val interpreters = new Array[interpreter](numInterpreters)
  val s = new Array[Sequence](numSequences)
  val t = new Array[Track](numSequences)
  val curr = 0

  var sequencer : Sequencer = MidiSystem.getSequencer()



  def InitializeInterpreters() {
    /* INTERPRETER KEY:
     * 0) Major
     * 1) Minor
     * 2) Dorian (Medieval)
     * 3) Middle-Eastern (happy)
     * 4) Middle-Eastern (sad)
     */

    interpreters(0) = new interpreter
    interpreters(0).initializeInterpreter(key.Minor)
    var x : ArrayBuffer[Int] = interpreters(0).weightDistributor(150, 30)
    InterpretCode(0, x)
  }

  /*
    This function applys the scales to the code
      interNum is the interpritor key
      code is the array of frequencies in order
   */
  def InterpretCode(interNum: Int, code: ArrayBuffer[Int]) {
    var i = 0
    var j = 0
    var count = 0
    val inter = interpreters(interNum)
    var notes = new ArrayBuffer[Int]
    for (i <- 0 to code.size - 1) {
      notes = inter.getNotes(code(i))._2
      for (j <- 0 to notes.size - 1) {
         GenerateNote(50 + notes(j),count,inter.getNotes(code(i))._1,100)
      }
      count += inter.getNotes(code(i))._1
    }
  }

  def BeginNote(msg: ShortMessage, start: Int, note: Int, volume: Int) {
        msg.setMessage(ShortMessage.NOTE_ON,0,note,volume)
        var event = new MidiEvent(msg,start)
        t(curr).add(event)
  }

  def EndNote(msg: ShortMessage, note: Int, end: Int) {
        msg.setMessage(ShortMessage.NOTE_OFF,0,note)
        var event = new MidiEvent(msg, end)
        t(curr).add(event)
  }

  def GenerateNote(note: Int, start: Int, duration: Int, volume: Int) {

        var beg = new ShortMessage()
        BeginNote(beg, start, note, volume)

        var end = new ShortMessage()
        EndNote(end, note, start + duration)
  }

  def BeginSequence() {
      sequencer.open()
      sequencer.setTempoFactor(4)
      sequencer.setSequence(s(curr))
      sequencer.start()
  }

  def EndSequence() {
      sequencer.close()
  }

  def PlaySong() {
      BeginSequence
      while(sequencer.isRunning()){
        Thread.sleep(1)
      }
      EndSequence
  }

  def SaveSong() {
     val song = new File("song.midi")
     MidiSystem.write(s(curr), 0, song)
   }

  def MakeSong() {
    // We'll need to edit this method heavily once we add the interpreters/clarify the structure
    var i = 0
    var numNotes = 30
    var x = 0
    for(x <- 0 to numNotes){
      var note = random.nextInt(3)*2 + 50
      var start = random.nextInt(numNotes) + i
      var duration = random.nextInt(numNotes - start) + 1
      var volume = 100
      GenerateNote(note, start, duration, volume)
    }
  }

  def StartCode(code:String) = {

    val hashMap : collection.mutable.HashMap[Char, Int] = parseFrequencies(code)
    val minHeap : collection.mutable.PriorityQueue[(Int, Node)] = buildHeap(hashMap)
    val root : Node = buildTree(minHeap)
    fillEncoding(root)
    printTree(root)
    val encodingMap : collection.mutable.HashMap[Char, String] = getEncodingHashMap(root)
    val byteString : String = getByteString(code, encodingMap)
    val byteArray  : Array[Byte] = convertStreamToByteArray(byteString)
    val bos : BufferedOutputStream = new BufferedOutputStream(new FileOutputStream("encoded.huff "))
    bos.write(byteArray)
    bos.close()

  }

  def EndCode(): Unit = {
    // hand nick a list of frequencies in an array
    val hashMap : collection.mutable.HashMap[Char, Int] = parseFrequencies(code)
    buildFrequencyRanking(hashMap) // Transforms it into a ranking not an absolute
//    InterpretCode(0, code)
  }

  class SongProperties() extends Dynamic {

  }

  object Initialize extends Dynamic {
    var num = 0
    def next(num : Int) = {
      numSequences = num


      InitializeGetter
    }

    def as(st : String) {
      if (st.equals("major")) {
        interpreters(0).initializeInterpreter(key.Major)
        var x : ArrayBuffer[Int] = interpreters(0).weightDistributor(150, 30)
        InterpretCode(0, x)
      }
    }

  }


  object InitializeGetter {
    def Interpreters(a : Any) = {
      interpreters(0) = new interpreter
        interpreters(0).initializeInterpreter(key.Major)
        var x : ArrayBuffer[Int] = interpreters(0).weightDistributor(150, 30)
        InterpretCode(0, x)
        KeyGetter
    }
    def sequences() = {
      var i = 0
      for (i <- 0 to numSequences - 1) {
        s(i) = new Sequence(Sequence.PPQ, 1)
        t(i) = s(i).createTrack()
      }
    }

  }

  object KeyGetter {
    def as(s : String) {
      if (s.equals("major")) {
        interpreters(0).initializeInterpreter(key.Major)
      } else if (s.equals("minor")) {
        interpreters(0).initializeInterpreter(key.Minor)
      } else if (s.equals("lydian")) {
        interpreters(0).initializeInterpreter(key.Lydian)
      }
    }
  }

  def parseFrequencies(code: String): collection.mutable.HashMap[Char,Int] = {

    val frequency_map = new collection.mutable.HashMap[Char,Int]()  { override def default(key:Char) = 0 }
    for (x <- 0 until code.length) {
      frequency_map += (code.charAt(x) -> (frequency_map(code.charAt(x)) + 1))
    }
    frequency_map

  }

  class Node (v: Int, c: Char, l: Node, r: Node) {
    /* Will be the class that is used to build up tree
    *
    * var left_node : Node
    * var right_node : Node
    * var value : Char
    * var encoding : String
    *
    * */
    val l_node : Node = l
    val r_node : Node = r
    val char : Char = c
    val value : Int = v
    var encoding : String = _

  }

  def buildHeap (map : collection.mutable.HashMap[Char,Int]) : collection.mutable.PriorityQueue[(Int,Node)] = {
    val minHeap = collection.mutable.PriorityQueue.empty(Ordering.by((_: (Int, Node))._1).reverse)
    for (x <- map.keySet.iterator) {
      val value : Int = map(x)
      val node : Node = new Node(value, x, null, null)
      minHeap += Tuple2(value, node)
    }
    minHeap
  }

  def buildFrequencyRanking(map : collection.mutable.HashMap[Char, Int]) : Unit = {
    val maxHeap = collection.mutable.PriorityQueue.empty(Ordering.by((_: (Int, Char))._1))
    for (x <- map.keySet.iterator) {
      val value : Int = map(x)
      maxHeap += Tuple2(value, x)
    }
    var rank : Int = 1
    while (!maxHeap.isEmpty) {
      val tup = maxHeap.dequeue()
      map += Tuple2(tup._2, rank)
      rank += 1
    }
  }

  def buildTree (heap : collection.mutable.PriorityQueue[(Int, Node)]) : Node = {
    while (heap.length != 1) {
      val first: (Int, Node) = heap.dequeue()
      val second: (Int, Node) = heap.dequeue()
      val newValue: Int = first._1 + second._1
      val newNode: Node = new Node(newValue, Char.MinValue, second._2, first._2)
      heap += Tuple2(newValue, newNode)
    }
    val root : Node = heap.dequeue()._2
    root.encoding = ""
    root
  }


  def fillEncoding (n: Node) : Unit = {

    if (n.l_node != null && n.r_node == null) {
      n.l_node.encoding = n.encoding + "0"
      fillEncoding(n.l_node)
    }
    if (n.l_node == null && n.r_node != null) {
      n.r_node.encoding = n.encoding + "1"
      fillEncoding(n.r_node)
    }
    if (n.r_node != null && n.l_node != null) {
      n.l_node.encoding = n.encoding + "0"
      fillEncoding(n.l_node)
      n.r_node.encoding = n.encoding + "1"
      fillEncoding(n.r_node)
    }

  }

  def encodingHashMapHelper (n:Node, h: collection.mutable.HashMap[Char, String]): Unit = {

    if (n.l_node == null && n.r_node == null) {
      h += (n.char -> n.encoding)
    } else if (n.l_node != null && n.r_node != null) {
      encodingHashMapHelper(n.l_node, h)
      encodingHashMapHelper(n.r_node, h)
    } else if (n.l_node != null) {
      encodingHashMapHelper(n.l_node, h)
    } else if (n.r_node != null) {
      encodingHashMapHelper(n.r_node, h)
    }


  }

  def getEncodingHashMap (n: Node) : collection.mutable.HashMap[Char, String] = {
    val encoding_map = new collection.mutable.HashMap[Char,String]()
    encodingHashMapHelper(n, encoding_map)
    encoding_map
  }

  def getByteString (code: String, h: collection.mutable.HashMap[Char, String]) : String = {

    var byteStringBuilder = ""
    for (x<-0 until code.length) {
      byteStringBuilder += h(code(x))
    }
    byteStringBuilder

  }

  def convertStreamToByteArray (s: String): Array[Byte] = {

    var byteArray = new Array[Byte](s.length/8 + 1)
    for {i<-0 until s.length if (i % 8 == 0)} {
      var newByte : Int = 0
      for {j<-0 until 8 if (7-j)+i < s.length} {
        val c : Char = s(i+(7-j))
        val cValue: Int = c.asDigit
        val exponent: Int = scala.math.pow(2, j).asInstanceOf[Int]
        println("CVALUE: " + cValue + " EXPONENT: " + exponent)
        newByte += cValue * exponent
      }
      byteArray(i/8) = newByte.asInstanceOf[Byte]
      println(byteArray(i/8))
    }
    byteArray

  }

  def printTree (n: Node) : Unit = {
    if (n != null) {
      printTree(n.l_node)
      print(n.char + ": " + n.encoding + "\n")
      printTree(n.r_node)
    }
  }



}
