
import scala.collection.mutable.ArrayBuffer
import backgroundmusicDSL.backgroundmusicDSL
import scala.language.{postfixOps, implicitConversions}
import words._

object backgroundmusicrunner extends backgroundmusicDSL {
  def main(args: Array[String]): Unit = {
    
    var x = StartCode(
       "var x = 0; x += 23; return x"
    )
    //Convert x into song Example
    
    Initialize the interpreters
    Initialize tracks 0 to 4
    
    InterpretCode(0, x, 0)
    PlaySong(0)
    Console.readLine()
    InterpretCode(1, x, 1)
    //Interpret x on 0 using major
    PlaySong(1)
    Console.readLine()
    InterpretCode(2, x, 2)
    PlaySong(2)
    
    //Play 0
    //PlaySong
    //Interpret "example" using major on track 0
    //Interpret "example" using minor on track 1
    //Interpret "example" using lydian on track 2
    //...
    
    //Play 8 seconds of tracks 0 to 2 
    //favorite = userInput
    //Save track favorite as "sample.midi"
    
    
    //PlaySong
    //SaveSong
    println("all done")
    
  }
}