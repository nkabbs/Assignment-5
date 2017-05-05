
import scala.collection.mutable.ArrayBuffer
import backgroundmusicDSL.backgroundmusicDSL
import scala.language.{postfixOps, implicitConversions}
import words._

object backgroundmusicrunner extends backgroundmusicDSL {
  def main(args: Array[String]): Unit = {
    
    var x = Compress code "var x = 0; x += 23; print x"
    
    Initialize the interpreters
    Initialize tracks 0 to 9
    
    Interpret code x on 0 through 0
    Play track 0
    
    Wait until input
    
    Interpret code x on 1 through 1
    Play track 1
    
    Wait until input
    
    Interpret code x on 2 through 2
    Play track 2
    
    Wait until input
    
    Interpret code x on 3 through 3
    Play track 3
    
    Wait until input
    
    Interpret code x on 4 through 4
    Play track 4
    
    Wait until input
    
    Interpret code x on 5 through 5
    Play track 5
    
    Wait until input
    
    Interpret code x on 6 through 6
    Play track 6
    
    Wait until input
    
    Interpret code x on 7 through 7
    Play track 7
    
    Wait until input
    
    Interpret code x on 8 through 8
    Play track 8
    
    Wait until input
    
    Interpret code x on 9 through 9
    Play track 9
    
    var choice : Int = Console.readInt
    
    Save track 8 as "example.midi"
    
    
  }
}