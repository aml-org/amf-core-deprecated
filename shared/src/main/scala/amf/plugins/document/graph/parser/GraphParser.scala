package amf.plugins.document.graph.parser
import amf.core.model.document.BaseUnit
import amf.core.parser.SyamlParsedDocument
import org.yaml.model.YDocument

trait GraphParser {
  def canParse(document: SyamlParsedDocument): Boolean
  def parse(document: YDocument, location: String): BaseUnit
}
