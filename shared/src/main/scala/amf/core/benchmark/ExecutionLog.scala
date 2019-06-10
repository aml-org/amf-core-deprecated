package amf.core.benchmark

import java.util.Date

import scala.concurrent.Future
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


case class Log(stage: String, time: Long)

case class Execution(startTime: Long, endTime: Long, logs: Seq[Log]) {
  def log(stage: String, time: Long) = {
    // System.err.println(stage)
    copy(logs = logs ++ Seq(Log(stage, time)), endTime = time)
  }
  def finish(): Execution = copy(endTime = new Date().getTime)
}

@JSExportAll
@JSExportTopLevel("ExecutionLog")
object ExecutionLog {
  var executions: Seq[Execution] = Nil
  var current: Option[Execution] = None
  var stagesSeq: Seq[String] = Seq()
  var stagesStartTime: Map[String,Long] = Map()
  var stagesEndTime: Map[String,Long] = Map()

  def startStage(name: String) = {
    current.foreach { _ =>
      stagesStartTime += (name -> new Date().getTime)
      stagesSeq = stagesSeq ++ Seq(name + "::SEP::start")
    }
  }

  def endStage(name: String) = {
    current.foreach { _ =>
      stagesEndTime += (name -> new Date().getTime)
      stagesSeq = stagesSeq ++ Seq(name + "::SEP::end")
    }
  }

  def withStage[X](name: String)(fn: => X):X = {
    startStage(name)
    fn match {
      case f: Future[_] =>
        val adapted = f.map { r =>
          endStage(name)
          r
        }
        adapted.asInstanceOf[X]
      case other        =>
        endStage(name)
        other
    }
  }

  def printStages() = {
    collectStages(stagesSeq).foreach { stageMessage =>
      System.err.println(stageMessage)
    }
  }

  protected def collectStages(remaining: Seq[String] = stagesSeq, level: Int = 0, isOpen: Boolean = false): Seq[String] = {
    remaining.headOption match {
      case Some(stage_timestamp) =>
        var parts = stage_timestamp.split("::SEP::")
        var stage = parts(0)
        var status = parts(1)
        status match {
          case "start" =>
            // nested
            val newLevel = if (isOpen) {
              level + 2
            } else {
              level
            }
            val nested = remaining.tail.takeWhile(!_.startsWith(s"$stage::SEP::end"))
            val nestedMessages = collectStages(nested, newLevel, isOpen = true)

            // remaining
            val remainingTraces =  remaining.tail.dropWhile(t => !t.startsWith(s"$stage::SEP::end")).tail
            val remainingMessages =  collectStages(remainingTraces, level, isOpen = true)

            // current
            val elapsed = stagesEndTime(stage) - stagesStartTime(stage)
            val stageMessage = (" " * level) ++ s"| [${elapsed} ms] $stage"

            Seq(stageMessage) ++ nestedMessages ++ remainingMessages
          case "end" =>
            Nil
        }
      case None =>
        Nil
    }
  }

  def log(stage: String): ExecutionLog.type = {
    current.foreach { execution =>
      val now = new Date().getTime
      current = Some(execution.log(stage, now))
    }
    this
  }

  def start(): ExecutionLog.type = {
    current.foreach { execution =>
      executions ++= Seq(execution.finish())
    }
    val now = new Date().getTime
    current = Some(Execution(now, now, Nil))

    this
  }

  def finish(): ExecutionLog.type = {
    current.foreach { execution =>
      executions ++= Seq(execution.finish())
    }
    current = None

    this
  }

  def buildReport() = {
    executions.zipWithIndex.foreach { case (execution, i) =>
      var prev = execution.startTime
      System.err.println(s"---- Run $i (${execution.endTime - execution.startTime} ms) ----\n")
      execution.logs.foreach { log =>
        System.err.println(s"   (${log.time - prev} ms) ${log.stage}")
        prev = log.time
      }
      System.err.println(s"   (${execution.endTime - prev} ms) Finished")
      System.err.println("\n\n\n")
    }
  }
}
