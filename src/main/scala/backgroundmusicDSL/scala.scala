package backgroundmusicDSL

import java.io.File
import javax.sound.midi._

import interpreter.{interpreter, key}

import scala.collection.mutable.ArrayBuffer
import scala.language.{dynamics, implicitConversions}
import scala.util.Random

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
    var root : Node = buildTree(minHeap)
    fillEncoding(root)
    printTree(root)
  }

  def EndCode(): Unit = {
    // hand nick a list of frequencies in an array
//    var arrayBufferFromTree = generateFrequencyArray();
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
    import collection.mutable.HashMap
    val frequency_map = new HashMap[Char,Int]()  { override def default(key:Char) = 0 }
    for (x <- 0 to code.length - 1) {
      frequency_map += (code.charAt(x) -> (frequency_map(code.charAt(x)) + 1))
    }
    return frequency_map
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

  def printTree (n: Node) : Unit = {
    if (n != null) {
      printTree(n.l_node)
      print(n.char + ": " + n.encoding + "\n")
      printTree(n.r_node)
    }
  }



}
