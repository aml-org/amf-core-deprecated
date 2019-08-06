package amf.core.model.domain.extensions

import amf.core.metamodel.domain.extensions.DomainExtensionModel.Extension
import amf.core.model.domain.{DataNode, DomainElement, Shape}

trait Extension extends DomainElement {
  def obtainSchema: Shape
  def extension: DataNode = fields.field(Extension)
}
