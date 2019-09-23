package amf.client.render

import amf.client.resolve.ClientErrorHandler
import amf.client.resolve.ClientErrorHandlerConverter._
import amf.core.parser.UnhandledErrorHandler

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

/**
  * Render options
  */
@JSExportAll
@JSExportTopLevel("render.RenderOptions")
class RenderOptions {

  private var sources: Boolean       = false
  private var compactUris: Boolean   = false
  private var amfJsonLdSerialization = true
  private var prettyPrint            = false
  private var flattenedJsonLd        = true
  private var eh: ClientErrorHandler = ErrorHandlerConverter.asClient(UnhandledErrorHandler)
  private var emitNodeIds            = false

  /** Pretty print the graph. */
  def withPrettyPrint: RenderOptions = {
    prettyPrint = true
    this
  }

  /** Not Pretty print the graph. */
  def withoutPrettyPrint: RenderOptions = {
    prettyPrint = false
    this
  }

  /** Include source maps when rendering to graph. */
  def withSourceMaps: RenderOptions = {
    sources = true
    this
  }

  /** Include source maps when rendering to graph. */
  def withoutSourceMaps: RenderOptions = {
    sources = false
    this
  }

  def withCompactUris: RenderOptions = {
    compactUris = true
    this
  }

  def withoutCompactUris: RenderOptions = {
    compactUris = false
    this
  }

  def withErrorHandler(errorHandler: ClientErrorHandler): RenderOptions = {
    eh = errorHandler
    this
  }

  def withFlattenedJsonLd: RenderOptions = {
    flattenedJsonLd = true
    this
  }

  def withoutFlattenedJsonLd: RenderOptions = {
    flattenedJsonLd = false
    this
  }

  def isFlattenedJsonLd: Boolean = flattenedJsonLd

  /**
    * Emit specific AMF JSON-LD serialization
    *
    * @return
    */
  def withoutAmfJsonLdSerialization: RenderOptions = {
    amfJsonLdSerialization = false
    this
  }

  /**
    * Emit regular JSON-LD serialization
    *
    * @return
    */
  def withAmfJsonLdSerialization: RenderOptions = {
    amfJsonLdSerialization = true
    this
  }

  def withNodeIds: RenderOptions = {
    emitNodeIds = true
    this
  }

  def isWithCompactUris: Boolean       = compactUris
  def isWithSourceMaps: Boolean        = sources
  def isAmfJsonLdSerilization: Boolean = amfJsonLdSerialization
  def errorHandler: ClientErrorHandler = eh
  def isPrettyPrint: Boolean           = prettyPrint
  def isEmitNodeIds: Boolean           = emitNodeIds
}

object RenderOptions {
  def apply(): RenderOptions = new RenderOptions()
}
