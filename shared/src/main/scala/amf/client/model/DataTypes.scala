package amf.client.model

import amf.core.model.DataType
import scala.scalajs.js.annotation.{JSExportAll, JSExportTopLevel}

@JSExportAll
@JSExportTopLevel("client.model.DataTypes")
object DataTypes {
  val String: String       = DataType.String
  val Integer: String      = DataType.Integer
  val Number: String       = DataType.Number
  val Long: String         = DataType.Long
  val Double: String       = DataType.Double
  val Float: String        = DataType.Float
  val Decimal: String      = DataType.Decimal
  val Boolean: String      = DataType.Boolean
  val Date: String         = DataType.Date
  val Time: String         = DataType.Time
  val DateTime: String     = DataType.DateTime
  val DateTimeOnly: String = DataType.DateTimeOnly
  val File: String         = DataType.File
  val Byte: String         = DataType.Byte
  val Binary: String       = DataType.Binary
  val Password: String     = DataType.Password
  val Any: String          = DataType.Any
  val AnyUri: String       = DataType.AnyUri
  val Nil: String          = DataType.Nil
}
