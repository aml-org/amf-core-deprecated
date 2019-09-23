package amf.core.emitter

import amf.client.render.{RenderOptions => ClientRenderOptions}
import amf.client.resolve.ClientErrorHandlerConverter._
import amf.core.metamodel.Field
import amf.core.parser.{ErrorHandler, UnhandledErrorHandler}

/**
  * Render options
  */
class RenderOptions {

  private var sources: Boolean               = false
  private var compactUris: Boolean           = false
  private var rawSourceMaps: Boolean         = false
  private var validating: Boolean            = false
  private var filterFields: Field => Boolean = (_: Field) => false
  private var amfJsonLdSerialization         = true
  private var useJsonLdEmitter               = false
  private var flattenedJsonLd                = true
  private var eh: ErrorHandler               = UnhandledErrorHandler
  private var prettyPrint                    = false
  private var emitNodeIds                    = false

  def withPrettyPrint: RenderOptions = {
    prettyPrint = true
    this
  }

  def withoutPrettyPrint: RenderOptions = {
    prettyPrint = true
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

  def withRawSourceMaps: RenderOptions = {
    rawSourceMaps = true
    this
  }

  def withoutRawSourceMaps: RenderOptions = {
    rawSourceMaps = false
    this
  }

  def withValidation: RenderOptions = {
    validating = true
    this
  }

  def withNodeIds: RenderOptions = {
    emitNodeIds = true
    this
  }

  def withoutNodeIds: RenderOptions = {
    emitNodeIds = false
    this
  }
  def withoutValidation: RenderOptions = {
    validating = false
    this
  }

  def withFilterFieldsFunc(f: Field => Boolean): RenderOptions = {
    filterFields = f
    this
  }

  def withoutAmfJsonLdSerialization: RenderOptions = {
    amfJsonLdSerialization = false
    this
  }

  def withAmfJsonLdSerialization: RenderOptions = {
    amfJsonLdSerialization = true
    this
  }

  def withErrorHandler(errorHandler: ErrorHandler): RenderOptions = {
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

  def isCompactUris: Boolean             = compactUris
  def isWithSourceMaps: Boolean          = sources
  def isWithRawSourceMaps: Boolean       = rawSourceMaps
  def isAmfJsonLdSerilization: Boolean   = amfJsonLdSerialization
  def isValidation: Boolean              = validating
  def renderField(field: Field): Boolean = !filterFields(field)
  def errorHandler: ErrorHandler         = eh
  def isPrettyPrint: Boolean             = prettyPrint
  def isEmitNodeIds: Boolean             = emitNodeIds
}

object RenderOptions {
  def apply(): RenderOptions = new RenderOptions()

  def apply(client: ClientRenderOptions): RenderOptions = {
    val opts = new RenderOptions()
    opts.sources = client.isWithSourceMaps
    opts.amfJsonLdSerialization = client.isAmfJsonLdSerilization
    opts.compactUris = client.isWithCompactUris
    opts.withErrorHandler(ErrorHandlerConverter.asInternal(client.errorHandler))
    opts.prettyPrint = client.isPrettyPrint
    opts.flattenedJsonLd = client.isFlattenedJsonLd
    opts
  }
}
