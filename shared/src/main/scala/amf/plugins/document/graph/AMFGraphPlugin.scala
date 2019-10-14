package amf.plugins.document.graph

import amf.client.plugins.{AMFDocumentPlugin, AMFPlugin}
import amf.core.Root
import amf.core.client.ParsingOptions
import amf.core.emitter.{RenderOptions, ShapeRenderOptions}
import amf.core.metamodel.Obj
import amf.core.metamodel.domain._
import amf.core.model.document.BaseUnit
import amf.core.model.domain.AnnotationGraphLoader
import amf.core.parser._
import amf.core.rdf.{RdfModelDocument, RdfModelParser}
import amf.core.remote.{Amf, Platform}
import amf.core.resolution.pipelines.{BasicResolutionPipeline, ResolutionPipeline}
import amf.core.unsafe.PlatformSecrets
import amf.plugins.document.graph.emitter.JsonLdEmitter
import amf.plugins.document.graph.parser.{ExpandedGraphParser, FlattenedGraphParser, GraphDependenciesReferenceHandler}
import org.yaml.builder.DocBuilder
import org.yaml.model.YDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object AMFGraphPlugin extends AMFDocumentPlugin with PlatformSecrets {

  override def init(): Future[AMFPlugin] = Future { this }

  override val ID: String     = Amf.name
  override def dependencies() = Seq()

  val vendors: Seq[String] = Seq(Amf.name)

  override def modelEntities: Seq[Obj] = Seq(
    ObjectNodeModel,
    ScalarNodeModel,
    ArrayNodeModel,
    LinkNodeModel,
    RecursiveShapeModel
  )

  override def serializableAnnotations(): Map[String, AnnotationGraphLoader] = Map.empty

  override def documentSyntaxes = Seq(
    "application/ld+json",
    "application/json",
    "application/amf+json"
  )

  override def canParse(root: Root): Boolean = {
    root.parsed match {
      case parsed: SyamlParsedDocument =>
        FlattenedGraphParser.canParse(parsed) || ExpandedGraphParser.canParse(parsed)
      case _: RdfModelDocument => true

      case _ => false
    }
  }

  override def parse(root: Root, ctx: ParserContext, platform: Platform, options: ParsingOptions): Option[BaseUnit] =
    root.parsed match {
      case parsed: SyamlParsedDocument if FlattenedGraphParser.canParse(parsed) =>
        Some(FlattenedGraphParser(platform).parse(parsed.document, effectiveUnitUrl(root.location, options)))
      case parsed: SyamlParsedDocument if ExpandedGraphParser.canParse(parsed) =>
        Some(ExpandedGraphParser(platform).parse(parsed.document, effectiveUnitUrl(root.location, options)))
      case parsed: RdfModelDocument =>
        Some(new RdfModelParser(platform)(ctx).parse(parsed.model, effectiveUnitUrl(root.location, options)))
      case _ =>
        None
    }

  override def canUnparse(unit: BaseUnit) = true

  override def emit[T](unit: BaseUnit,
                       builder: DocBuilder[T],
                       renderOptions: RenderOptions,
                       shapeRenderOptions: ShapeRenderOptions = ShapeRenderOptions()): Boolean =
    JsonLdEmitter.emit(unit, builder, renderOptions)

  override protected def unparseAsYDocument(
      unit: BaseUnit,
      renderOptions: RenderOptions,
      shapeRenderOptions: ShapeRenderOptions = ShapeRenderOptions()): Option[YDocument] =
    throw new IllegalStateException("Unreachable")

  override def referenceHandler(eh: ErrorHandler): ReferenceHandler = GraphDependenciesReferenceHandler

  /**
    * Resolves the provided base unit model, according to the semantics of the domain of the document
    */
  override def resolve(unit: BaseUnit,
                       errorHandler: ErrorHandler,
                       pipelineId: String = ResolutionPipeline.DEFAULT_PIPELINE): BaseUnit =
    new BasicResolutionPipeline(errorHandler).resolve(unit)

  /**
    * Does references in this type of documents be recursive?
    */
  override val allowRecursiveReferences: Boolean = true

  protected def effectiveUnitUrl(location: String, options: ParsingOptions): String = {
    options.definedBaseUrl match {
      case Some(url) => url
      case None      => location
    }
  }

}
