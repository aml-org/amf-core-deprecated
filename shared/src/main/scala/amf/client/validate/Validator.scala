package amf.client.validate

import amf.client.convert.CoreClientConverters._
import amf.client.environment.{DefaultEnvironment, Environment}
import amf.client.model.document.BaseUnit
import amf.core.errorhandling.UnhandledErrorHandler
import amf.core.services.RuntimeValidator
import amf.{AMFStyle, MessageStyle, ProfileName}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.JSExportAll

@JSExportAll
object Validator {

  def validate(model: BaseUnit,
               profileName: ProfileName,
               messageStyle: MessageStyle = AMFStyle,
               env: Environment = DefaultEnvironment(),
               resolved: Boolean = false): ClientFuture[ValidationReport] =
    RuntimeValidator(
      model._internal,
      profileName,
      messageStyle,
      env._internal,
      resolved
    ).map(report => report).asClient

  def loadValidationProfile(url: String,
                            env: Environment = DefaultEnvironment()): ClientFuture[ProfileName] =
    RuntimeValidator.loadValidationProfile(url, env._internal, UnhandledErrorHandler).asClient

  def emitShapesGraph(profileName: ProfileName): String =
    RuntimeValidator.emitShapesGraph(profileName)
}
