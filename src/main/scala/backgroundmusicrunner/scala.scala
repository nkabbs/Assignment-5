import backgroundmusicDSL.backgroundmusicDSL

object backgroundmusicrunner extends backgroundmusicDSL {
  def main(args: Array[String]): Unit = {
    InitializeSequences
    StartCode("var number = 11; number = number + 1;")
    EndCode
    MakeSong
    println("Turn up your speakers, I am playing music")
    PlaySong
    SaveSong
    println("all done")
    
  }
}