package amf

import amf.core.AMF
import amf.core.client.{Generator, Parser, Resolver, Validator}
import amf.core.plugins.AMFPlugin
import amf.core.unsafe.PlatformSecrets
import amf.model.document._
import amf.model.domain._
import amf.validation.AMFValidationReport

import scala.scalajs.js.JSConverters._
import scala.scalajs.js.Promise
import scala.scalajs.js.annotation.JSExportAll
import scala.concurrent.ExecutionContext.Implicits.global

@JSExportAll
object Core extends PlatformSecrets {

  def init(): Promise[Unit] = {
    platform.registerWrapper(amf.core.metamodel.document.ModuleModel) {
      case m: amf.core.model.document.Module => Module(m)
    }
    platform.registerWrapper(amf.core.metamodel.document.DocumentModel) {
      case m: amf.core.model.document.Document => Document(m)
    }
    platform.registerWrapper(amf.core.metamodel.document.FragmentModel) {
      case f: amf.core.model.document.Fragment => new Fragment(f)
    }
    platform.registerWrapper(amf.core.metamodel.document.ExternalFragmentModel) {
      case f: amf.core.model.document.ExternalFragment => ExternalFragment(f)
    }
    platform.registerWrapper(amf.core.metamodel.domain.DomainElementModel) {
      case e: amf.core.model.domain.DomainElement => DomainElement(e)
    }
    platform.registerWrapper(amf.core.metamodel.domain.extensions.CustomDomainPropertyModel) {
      case e: amf.core.model.domain.extensions.CustomDomainProperty => CustomDomainProperty(e)
    }
    platform.registerWrapper(amf.core.metamodel.domain.extensions.DomainExtensionModel) {
      case e: amf.core.model.domain.extensions.DomainExtension => DomainExtension(e)
    }
    platform.registerWrapper(amf.core.metamodel.domain.extensions.PropertyShapeModel) {
      case e: amf.core.model.domain.extensions.PropertyShape => PropertyShape(e)
    }
    platform.registerWrapper(amf.core.metamodel.domain.DataNodeModel) {
      case o: amf.core.model.domain.ObjectNode => ObjectNode(o)
      case s: amf.core.model.domain.ScalarNode => ScalarNode(s)
      case a: amf.core.model.domain.ArrayNode  => ArrayNode(a)
      case d: amf.core.model.domain.DataNode   => DataNode(d)
    }
    platform.registerWrapper(amf.core.metamodel.domain.templates.VariableValueModel) {
      case v: amf.core.model.domain.templates.VariableValue => VariableValue(v)
    }

    // Init the core component
    AMF.init().toJSPromise
  }

  def parser(vendor: String, mediaType: String): Parser       = new Parser(vendor, mediaType)
  def generator(vendor: String, mediaType: String): Generator = new Generator(vendor, mediaType)
  def resolver(vendor: String): Resolver                      = new Resolver(vendor)
  def validate(model: BaseUnit, profileName: String, messageStyle: String = "AMF"): Promise[AMFValidationReport] =
    Validator.validate(model, profileName, messageStyle)
  def loadValidationProfile(url: String): Promise[String]       = Validator.loadValidationProfile(url)
  def registerNamespace(alias: String, prefix: String): Boolean = platform.registerNamespace(alias, prefix).isDefined
  def registerPlugin(plugin: AMFPlugin): Unit                   = AMF.registerPlugin(plugin)
}
