import backgroundmusicDSL.backgroundmusicDSL

object backgroundmusicrunner extends backgroundmusicDSL {
  def main(args: Array[String]): Unit = {
    InitializeSequences
    MakeSong
    println("Turn up your speakers, I am playing music")
    PlaySong
    SaveSong
    println("all done")
    
  }
}