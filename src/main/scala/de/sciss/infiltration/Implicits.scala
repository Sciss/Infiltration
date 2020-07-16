package de.sciss.infiltration

import java.util.{Timer, TimerTask}

import de.sciss.processor.{Processor, ProcessorLike}

import scala.concurrent.{ExecutionContext, Future}

object Implicits {
  implicit final class ProcessorOps[A](private val p: ProcessorLike[A, Any] with Processor.Prepared)
    extends AnyVal {

    def startWithTimeout(dur: Double)(implicit timer: Timer, exec: ExecutionContext): Future[A] = {
      p.start()
      val ttTimeOut = new TimerTask {
        def run(): Unit = p.abort()
      }
      timer.schedule(ttTimeOut, (dur * 1000).toLong)
      p.transform { tr =>
        ttTimeOut.cancel()
        tr
      }
    }
  }
}
