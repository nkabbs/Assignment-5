import backgroundmusicDSL.backgroundmusicDSL

object backgroundmusicrunner extends backgroundmusicDSL {
  def main(args: Array[String]): Unit = {
    Initialize next 3 sequences; 
    Initialize next 1 Interpreters 0 as "lydian";
    StartCode("var x = 0; x += 23; return x;")
    EndCode
    //Initialize a set of 3 sequences;
    //Initialize an interpreter using the lydian setting;
    //Make a distribution "d1" using 30 notes from 150 characters;
    //Make song "sample" from distribution(0) on track 0.
    //Save song "sample" as "sample.midi"
    PlaySong
    SaveSong
    println("all done")
  }
}