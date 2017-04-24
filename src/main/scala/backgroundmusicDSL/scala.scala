package backgroundmusicDSL

import java.io.File
import javax.sound.midi._
import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import interpreter.interpreter
import interpreter.key

class backgroundmusicDSL {
  def hello : String = "Hello World!"
  val random = new Random 
  
  val numSequences = 10
  val numInterpreters = 5
  
  val interpreters = new Array[interpreter](numInterpreters)
  val s = new Array[Sequence](numSequences)
  val t = new Array[Track](numSequences)
  val curr = 0
  
  var sequencer : Sequencer = MidiSystem.getSequencer()
  
  def InitializeSequences() {
    var i = 0
    for (i <- 0 to numSequences - 1) {
      s(i) = new Sequence(Sequence.PPQ, 1)
      t(i) = s(i).createTrack()
    }
  }
  
  
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

  def StartCode(): Unit = {

  }

  def EndCode(): Unit = {

  }
  
}