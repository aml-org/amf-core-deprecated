package amf.client.model.domain

import amf.client.convert.CoreClientConverters._
import amf.client.model.AmfObjectWrapper
import amf.core.model.domain.{DomainElement => InternalDomainElement}
import amf.core.parser.Range
import amf.core.unsafe.PlatformSecrets

import scala.scalajs.js.annotation.JSExportAll

/**
  * Domain element.
  */
@JSExportAll
trait DomainElement extends AmfObjectWrapper with PlatformSecrets {

  override private[amf] val _internal: InternalDomainElement

  def customDomainProperties: ClientList[DomainExtension] = _internal.customDomainProperties.asClient
  def extendsNode: ClientList[DomainElement]              = _internal.extend.asClient
  def id: String                                          = _internal.id
  def position: Range                                     = _internal.position().orNull

  def withCustomDomainProperties(extensions: ClientList[DomainExtension]): this.type = {
    _internal.withCustomDomainProperties(extensions.asInternal)
    this
  }

  def withExtendsNode(extension: ClientList[ParametrizedDeclaration]): this.type = {
    _internal.withExtends(extension.asInternal)
    this
  }

  def withId(id: String): this.type = {
    _internal.withId(id)
    this
  }

  def getTypeIds: ClientList[String] = _internal.getTypeIds().asClient

  def getPropertyIds: ClientList[String] = _internal.getPropertyIds().asClient

  def getScalarByPropertyId(id: String): ClientList[Any] = _internal.getScalarByPropertyId(id).asClient

  def getObjectByPropertyId(id: String): ClientList[DomainElement] = _internal.getObjectByPropertyId(id).asClient
}
