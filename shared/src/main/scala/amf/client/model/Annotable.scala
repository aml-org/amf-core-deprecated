package amf.client.model

import amf.core.parser.Annotations

trait Annotable {

  /** Return annotations. */
  def annotations(): Annotations
}
