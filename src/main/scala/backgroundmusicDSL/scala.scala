package backgroundmusicDSL

import scala.util.Random

import java.io.File
import com.sun.media.sound
import javax.sound._
import javax.sound.midi._
import javax.sound.midi.Instrument._

class backgroundmusicDSL {
  def hello : String = "Hello World!"
  val random = new Random 
  
  val numSequences = 10
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
    var numNotes = 50
    var x = 0
    for(x <- 0 to numNotes){
      var note = random.nextInt(3)*2 + 50
      var start = random.nextInt(numNotes) + i
      var duration = random.nextInt(numNotes - start) + 1
      var volume = 100
      GenerateNote(note, start, duration, volume)
    }
  }
  
}