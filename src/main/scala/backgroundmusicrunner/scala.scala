import backgroundmusicDSL.backgroundmusicDSL

object backgroundmusicrunner extends backgroundmusicDSL {
  def main(args: Array[String]): Unit = {
    InitializeSequences
    InitializeInterpreters
    PlaySong
    SaveSong
    println("all done")
    
  }
}