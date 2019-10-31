package amf.plugins.document.graph.emitter.flattened.utils

import org.mulesoft.lexer.Queue
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

  private val queue: Queue[Emission[T] with Metadata] = new Queue()
  // Ids in queue or previously in queue
  private val knownIds: mutable.HashSet[String] = mutable.HashSet[String]()

  def tryEnqueue(e: Emission[T] with Metadata): Try[Unit] = {
    if (accepts(e)) {
      queue += e
      e.id.map { id =>
        knownIds += id
      }
      Success()
    } else {
      Failure(new IllegalArgumentException("Element already emitted"))
    }
  }

  def hasPendingEmissions: Boolean = !queue.isEmpty

  def nextEmission(): Emission[T] with Metadata = {
    val next = queue.dequeue
    next
  }

  /**
    *   Returns true if an emission should be added to the queue
    *
    * @param e emission
    * @return
    */
  def accepts(e: Emission[T] with Metadata): Boolean = !e.id.exists { id => knownIds.contains(id) }

}
