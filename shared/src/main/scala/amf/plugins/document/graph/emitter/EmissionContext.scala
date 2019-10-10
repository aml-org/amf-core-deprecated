package amf.plugins.document.graph.emitter

import amf.core.emitter.RenderOptions
import amf.core.model.document.BaseUnit
import amf.core.model.domain.{AmfElement, AmfObject}
import amf.core.utils.IdCounter
import amf.core.vocabulary.Namespace
import org.yaml.builder.DocBuilder.Entry
import org.yaml.model.YDocument.EntryBuilder

import scala.collection.mutable

class EmissionContext(val prefixes: mutable.Map[String, String],
                      var base: String,
                      val options: RenderOptions,
                      var emittingDeclarations: Boolean = false) {
  var counter: Int = 1

  private val declarations: mutable.LinkedHashSet[AmfElement] = mutable.LinkedHashSet.empty

  private val typeCount: IdCounter = new IdCounter()

  def nextTypeName: String = typeCount.genId("amf_inline_type")

  def emittingDeclarations(d: Boolean): this.type = {
    emittingDeclarations = d
    this
  }

  def +(element: AmfElement): this.type = {
    declarations += element
    this
  }

  def ++(elements: Iterable[AmfElement]): this.type = {
    declarations ++= elements
    this
  }

  def isDeclared(e: AmfElement): Boolean = declarations.contains(e)

  def isDeclared(id: String): Boolean =
    declarations.collect({ case obj: AmfObject if obj.id.equals(id) => obj }).nonEmpty

  def declared: Seq[AmfElement] = declarations.toSeq

  def shouldCompact: Boolean = options.isCompactUris

  protected def compactAndCollect(uri: String): String = Namespace.compactAndCollect(uri, prefixes)

  def emitIri(uri: String): String = if (shouldCompact) compactAndCollect(uri) else uri

  def emitId(uri: String): String = if (shouldCompact && uri.contains(base)) uri.replace(base, "") else uri

  def setupContextBase(location: String): Unit = {
    if (Option(location).isDefined) {
      base = if (location.replace("://", "").contains("/")) {
        val basePre = if (location.contains("#")) {
          location.split("#").head
        } else {
          location
        }
        val parts = basePre.split("/").dropRight(1)
        parts.mkString("/")
      } else {
        location.split("#").head
      }
    } else {
      base = ""
    }
  }

  def emitContext[T](b: Entry[T]): Unit = {
    if (shouldCompact)
      b.entry("@context", _.obj { b =>
        b.entry("@base", base)
        prefixes.foreach {
          case (p, v) =>
            b.entry(p, v)
        }
      })
  }

  def emitContext(b: EntryBuilder): Unit = {
    if (shouldCompact)
      b.entry("@context", _.obj { b =>
        b.entry("@base", base)
        prefixes.foreach {
          case (p, v) =>
            b.entry(p, v)
        }
      })
  }
}

object EmissionContext {
  def apply(unit: BaseUnit, options: RenderOptions) =
    new EmissionContext(mutable.Map(), unit.id, options)
}

class FlattenedEmissionContext(prefixes: mutable.Map[String, String],
                               base: String,
                               options: RenderOptions,
                               emittingDeclarations: Boolean = false)
    extends EmissionContext(prefixes, base, options, emittingDeclarations) {

  override def emitId(uri: String): String = {
    if (shouldCompact) {
      if (uri == base) "./"
      else uri.replace(base, "")
    } else {
      uri
    }
  }
}

object FlattenedEmissionContext {
  def apply(unit: BaseUnit, options: RenderOptions) = new FlattenedEmissionContext(mutable.Map(), unit.id, options)
}
