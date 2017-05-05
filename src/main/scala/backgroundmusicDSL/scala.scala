package backgroundmusicDSL

import words._
import java.io.{BufferedOutputStream, File, FileOutputStream}
import javax.sound.midi._

import interpreter.{interpreter, key}

import scala.collection.mutable.ArrayBuffer
import scala.language.{dynamics, implicitConversions}
import scala.util.Random
import collection.mutable.HashMap


class backgroundmusicDSL {
  
  val random = new Random //used for generating sample songs not based on specific encoding
  
  var numSequences = 10  //number of available tracks to write on. can't be changed
  val numInterpreters = 10  //== numInterpreters
  
  val interps = new Array[interpreter](numInterpreters)  //interps is just a list of interpreters, but named interps so interpreters can be used as a word in the DSL
  
  val s = new Array[Sequence](numSequences)
  val t = new Array[Track](numSequences)
  
  var currTrack = 0  //used for storing current track that the music is writing to
  var firstTrack = 0  //used for storing first track number to be initialized
  var c = new ArrayBuffer[Int]()  //used for storing current code that can be interpreted on tracks
  
  var sequencer : Sequencer = MidiSystem.getSequencer()
  
    /* INTERPRETER KEY:
     * 0) Major
     * 1) Minor
     * 2) Middle-Eastern (happy)
     * 3) Lydian 
     * 4) Dorian
     * 5) Messy
     * 6) WholeStep
     * 7) Blues
     * 8) PentatonicMajor
     * 9) PentatonicMinor
     */
  
  object Play {
    def track(i : Int) = {
      PlaySong(i)
    }
  }
  
  object Save {
    def track(i : Int) = {
      currTrack = i
      trackSaver
    }
  }
  
  object trackSaver {
    def as(s : String) {
      SaveSong(currTrack, s)
    }
  }
  
  object Initialize extends Dynamic {
    var num = 0
    def apply(num : Int) = {
      numSequences = num
    }
    
    
    def tracks(i : Int) = {
      firstTrack = i
      trackInitializer
      
    }
    
    def the(v : interpretersWord) = InitializeInterpreters
    
  }
  
  object trackInitializer {
    def to(lastTrack: Int)  {
      for (i <- firstTrack to lastTrack) {
          s(i) = new Sequence(Sequence.PPQ, 1)
          t(i) = s(i).createTrack()
        }
    }
  }
  
  object Interpret {
    def code(co : ArrayBuffer[Int]) = {
      c = co
      TrackGetter
    }
  }
  
  object TrackGetter {
    def on(i : Int) = {
      currTrack = i
      InterpreterGetter
    }
  }
  
  object InterpreterGetter {
    def through(i : Int) {
      InterpretCode(i, c, currTrack)
    }
  }
  
  object Compress {
    def code(s : String) = {
      StartCode(s)
    }
  }
  
  object Wait {
    def until(d : inputWord) {
      Console.readLine()
    }
  }
  
  
  def InitializeInterpreters() {
    var i = 0
    for (i <- 0 until numInterpreters) {
      interps(i) = new interpreter
      interps(i).initializeInterpreter(i)
      print (i)
    }
  }
  
  //Function that turns encoded music file into real MIDI file
  def InterpretCode(interNum: Int, code: ArrayBuffer[Int], trackNum : Int) {  //code passed in is frequency distribution
    var i = 0
    var j = 0
    var count = 0
    val inter = interps(interNum)
    var notes = new ArrayBuffer[Int]
    for (i <- 0 to code.size - 1) {
      notes = inter.getNotes(code(i))._2  //getNotes()._1 gets the list of notes played (since past the first 15 characters there can be multiple)
      println(notes(0))
      for (j <- 0 to notes.size - 1) { //loops through all notes to be played simultaneously
         GenerateNote(50 + notes(j),count,inter.getNotes(code(i))._1,100, trackNum) 
      }
      count += inter.getNotes(code(i))._1  //getNotes()._1 gets the duration of the notes
    }
  }
  
  def BeginNote(msg: ShortMessage, begin: Int, pitch: Int, volume: Int, track : Int) {
        msg.setMessage(ShortMessage.NOTE_ON,0,pitch,volume)
        var event = new MidiEvent(msg,begin)
        t(track).add(event)
  }
  
  def EndNote(msg: ShortMessage, pitch: Int, end: Int, track : Int) {
        msg.setMessage(ShortMessage.NOTE_OFF,0,pitch)
        var event = new MidiEvent(msg, end)
        t(track).add(event)
  }
  
  def GenerateNote(pitch: Int, begin: Int, noteLength: Int, volume: Int, track : Int) {
        
        var beg = new ShortMessage()
        BeginNote(beg, begin, pitch, volume, track)

        var end = new ShortMessage()
        EndNote(end, pitch, begin + noteLength, track)
  }
  
  def BeginSequence(x : Int) {
      sequencer.open()
      sequencer.setTempoFactor(2)
      sequencer.setSequence(s(x))
      sequencer.start()
  }
  
  def EndSequence() {
      sequencer.close()
  }
  
  def PlaySong(x : Int) {
      BeginSequence(x)
      print("here")
      while(sequencer.isRunning()){
        Thread.sleep(2000)  //Sleep count added so that sequence doesn't end before song is played
      }
      EndSequence
  }
  
  def SaveSong(seq : Int, fileName : String) {
     val song = new File(fileName)
     MidiSystem.write(s(seq), 0, song)
   }
  
  def MakeSong(tr : Int)  {  //generates a random distribution of 3 possible notes. Only used for testing
    var i = 0
    var numNotes = 30
    var x = 0
    for(x <- 0 to numNotes){
      var note = random.nextInt(3)*2 + 50
      var start = random.nextInt(numNotes) + i
      var duration = random.nextInt(numNotes - start) + 1
      var volume = 100
      GenerateNote(note, start, duration, volume, tr)
    }
  }

  //creates the encoded song file from frequency array and the original code
  def generateSongCode(code : String, map : HashMap[Char, Int]) : ArrayBuffer[Int] = {
    var i = 0
    var songCode = new ArrayBuffer[Int]
    for (i <- 0 until code.length()) {
      songCode.append(map.getOrElse(code(i),0))
    }
    return songCode
  }
  
  //maps each character to the number of its occurrences
  def buildFrequencyArr(map : collection.mutable.HashMap[Char, Int]) : ArrayBuffer[Int] = {
    var freqArr = ArrayBuffer[Int]()
    for (x <- map.keySet.iterator) {
      val value : Int = map(x)
      freqArr += value
    }
    freqArr.sorted
  }
  
  
  //Simultaneously compresses code and creates encoded music file
  def StartCode(code:String) : ArrayBuffer[Int] = {

    val hashMap : collection.mutable.HashMap[Char, Int] = parseFrequencies(code)
    val minHeap : collection.mutable.PriorityQueue[(Int, Node)] = buildHeap(hashMap)
    val root : Node = buildTree(minHeap)
    fillEncoding(root)
    //printTree(root)
    val encodingMap : collection.mutable.HashMap[Char, String] = getEncodingHashMap(root)
    val byteString : String = getByteString(code, encodingMap)
    val byteArray  : Array[Byte] = convertStreamToByteArray(byteString)
    val bos : BufferedOutputStream = new BufferedOutputStream(new FileOutputStream("encoded.huff "))
    bos.write(byteArray)
    bos.close()
    buildFrequencyRanking(hashMap)
    return generateSongCode(code, hashMap);  //returns encoded music file to be interpreted after compression
  }
  
  //ranks frequencies for purpose of Huffman coding and music encoding
  def buildFrequencyRanking(map : HashMap[Char, Int]) : Unit = {
    val maxHeap = collection.mutable.PriorityQueue.empty(Ordering.by((_: (Int, Char))._1))
    for (x <- map.keySet.iterator) {
      val value : Int = map(x)
      maxHeap += Tuple2(value, x)
    }
    var rank : Int = 0;
    while (!maxHeap.isEmpty) {
      val tup = maxHeap.dequeue()
      map += Tuple2(tup._2, rank)
      rank += 1
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
        newByte += cValue * exponent
      }
      byteArray(i/8) = newByte.asInstanceOf[Byte]
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
