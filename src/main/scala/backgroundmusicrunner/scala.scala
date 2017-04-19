import backgroundmusicDSL.backgroundmusicDSL

object backgroundmusicrunner extends backgroundmusicDSL {
  def main(args: Array[String]): Unit = {
    InitializeSequences
    MakeSong
    PlaySong
    SaveSong
    println("all done")
    
  }
}