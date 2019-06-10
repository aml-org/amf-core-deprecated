package amf.core.remote.browser

import amf.core.emitter.RenderOptions
import amf.core.model.document.BaseUnit
import amf.internal.resource.{ResourceLoader, ResourceLoaderAdapter}
import amf.core.remote._
import amf.plugins.document.graph.parser.ScalaJSJsonLdEmitter
import org.mulesoft.common.io.{FileSystem, Output}

import scala.scalajs.js.annotation.JSExportAll

/**
  *
  */
class JsBrowserPlatform extends JsPlatform {

  /** Underlying file system for platform. */
  override val fs: FileSystem = UnsupportedFileSystem

  override def loaders(): Seq[ResourceLoader] = Seq(ResourceLoaderAdapter(JsBrowserHttpResourceLoader()))

  /** Return temporary directory. */
  override def tmpdir(): String = {
    // Accept in Node only
    throw new Exception(s"Unsupported tmpdir operation")
  }

  override def operativeSystem(): String = "web"

  override def resolvePath(path: String): String = path

  override def emitJSON[W: Output](unit: BaseUnit, writer: W, renderOptions: RenderOptions): Boolean = {
    ScalaJSJsonLdEmitter.emit(unit, writer, renderOptions)
  }
}

@JSExportAll
object JsBrowserPlatform {
  private var singleton: Option[JsBrowserPlatform] = None

  def instance(): JsBrowserPlatform = singleton match {
    case Some(p) => p
    case None =>
      singleton = Some(new JsBrowserPlatform())
      singleton.get
  }
}
