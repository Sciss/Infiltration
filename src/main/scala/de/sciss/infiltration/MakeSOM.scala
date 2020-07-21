//package de.sciss.infiltration
//
//import de.sciss.lucre.stm
//import de.sciss.lucre.synth.Sys
//import de.sciss.negatum.Negatum.Evaluation
//import de.sciss.negatum.SOM
//import de.sciss.synth.proc.Durable
//
//object MakeSOM {
//  def main(args: Array[String]): Unit = {
//
//  }
//
//
//
//  private def run[S <: Sys[S]]()(implicit cursor: stm.Cursor[S]): Unit = {
//    val cfg = SOM.Config()
//    cfg.numIterations = 3000
//    cfg.features      = ???
//
//    cursor.step { implicit tx =>
//      Evaluation.apply()
//      val som = SOM[S](cfg)
//      som.add(???, ???)
//    }
//  }
//}
