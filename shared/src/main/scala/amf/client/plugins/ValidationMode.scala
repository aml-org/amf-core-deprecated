package amf.client.plugins

import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
trait ValidationMode

@JSExportAll
@JSExportTopLevel("ValidationMode")
object ValidationMode {
  val StrictValidationMode: ValidationMode        = amf.client.plugins.StrictValidationMode
  val ScalarRelaxedValidationMode: ValidationMode = amf.client.plugins.ScalarRelaxedValidationMode
}

@JSExportAll
object StrictValidationMode extends ValidationMode
@JSExportAll
object ScalarRelaxedValidationMode extends ValidationMode
