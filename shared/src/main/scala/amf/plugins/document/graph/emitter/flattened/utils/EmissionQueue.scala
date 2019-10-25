package amf.plugins.document.graph.emitter.flattened.utils

import org.yaml.builder.DocBuilder.Part

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class Emission[T](val fn: Part[T] => Unit)

trait Metadata {
  var id: Option[String] = None
  var isDeclaration: Boolean = false
}

/**
  * Queue used to obtain objects or emission
  *
  * @tparam T
  */
case class EmissionQueue[T]() {

  private val queue: mutable.Queue[Emission[T] with Metadata] = mutable.Queue.empty

  val pastIds: mutable.HashSet[String] = mutable.HashSet.empty

  def tryEnqueue(e: Emission[T] with Metadata): Try[Unit] = {
    if (accepts(e)) {
      queue.enqueue(e)
      Success()
    } else {
      Failure(new IllegalArgumentException("Element already emitted"))
    }
  }

  def hasPendingEmissions: Boolean = queue.nonEmpty

  def nextEmission(): Emission[T] with Metadata = {
    val next = queue.dequeue()
    next.id.map { id =>
      pastIds += id
    }
    next
  }

  /**
    *   Returns true if an emission should be added to the queue
    *
    * @param e emission
    * @return
    */
  def accepts(e: Emission[T] with Metadata): Boolean = {
    val inQueue = queue.contains(e)
    val previouslyInQueue = e.id.exists { id =>
      pastIds.contains(id)
    }
    !inQueue && !previouslyInQueue
  }

}
