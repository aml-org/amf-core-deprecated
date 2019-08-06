package amf.core.model.domain.extensions

import amf.core.metamodel.domain.extensions.ShapeExtensionModel
import amf.core.model.domain.{DataNode, DomainElement, Shape}
import amf.core.metamodel.domain.extensions.ShapeExtensionModel._
import amf.core.parser.{Annotations, Fields}
import org.yaml.model.YPart
import amf.core.utils.Strings

case class ShapeExtension(fields: Fields, annotations: Annotations) extends Extension {

  id = "http://a.ml/vocabularies#document/shape_extension"

  def definedBy: PropertyShape = fields.field(DefinedBy)
  def obtainSchema: Shape      = definedBy.range

  def withDefinedBy(customProperty: PropertyShape): this.type =
    set(DefinedBy, customProperty)

  def withExtension(extension: DataNode): this.type = set(Extension, extension)

  override def meta = ShapeExtensionModel

  // This element will never be serialised in the JSON-LD graph, it is just a placeholder
  // for the extension point. ID is not required for serialisation

  /** Value , path + field value that is used to compose the id when the object its adopted */
  override def componentId: String = "/shape-extension/" +
    Option(definedBy).flatMap(_.name.option()).getOrElse("default-extension").urlComponentEncoded
}

object ShapeExtension {
  def apply(): ShapeExtension = apply(Annotations())

  def apply(ast: YPart): ShapeExtension = apply(Annotations(ast))

  def apply(annotations: Annotations): ShapeExtension = ShapeExtension(Fields(), annotations)
}
