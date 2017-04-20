import backgroundmusicDSL.backgroundmusicDSL

object backgroundmusicrunner extends backgroundmusicDSL {
  def main(args: Array[String]): Unit = {
    InitializeSequences
    StartCode
      // insert scala code here
    EndCode
    MakeSong
    println("Turn up your speakers, I am playing music")
    PlaySong
    SaveSong
    println("all done")
    
  }
}