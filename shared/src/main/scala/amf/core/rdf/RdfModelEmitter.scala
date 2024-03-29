package amf.core.rdf

import amf.core.annotations.{DomainExtensionAnnotation, ScalarType}
import amf.core.emitter.RenderOptions
import amf.core.metamodel.Type.{Array, Bool, EncodedIri, Iri, LiteralUri, SortedArray, Str}
import amf.core.metamodel._
import amf.core.metamodel.document.SourceMapModel
import amf.core.metamodel.domain.extensions.DomainExtensionModel
import amf.core.metamodel.domain.{DomainElementModel, LinkableElementModel, ShapeModel}
import amf.core.model.DataType
import amf.core.model.document.{BaseUnit, SourceMap}
import amf.core.model.domain.DataNodeOps.adoptTree
import amf.core.model.domain._
import amf.core.model.domain.extensions.DomainExtension
import amf.core.parser.{Annotations, FieldEntry, Value}
import amf.core.traversal.ModelTraversalRegistry
import amf.core.vocabulary.{Namespace, ValueType}
import org.mulesoft.common.time.SimpleDateTime

import scala.collection.mutable.ListBuffer

/**
  * AMF RDF Model emitter
  */
class RdfModelEmitter(rdfmodel: RdfModel) extends MetaModelTypeMapping {

  def emit(unit: BaseUnit, options: RenderOptions): Unit = Emitter(options).root(unit)

  case class Emitter(options: RenderOptions) {

    private val traversal = ModelTraversalRegistry()
    var rootId: Option[String]     = None

    def root(unit: BaseUnit): Unit = {
      rootId = Some(unit.id)
      traverse(unit)
    }

    protected def selfEncoded(element: AmfObject): Boolean =
      element.id == rootId.getOrElse("") && !element.isInstanceOf[BaseUnit]

    def traverse(element: AmfObject): Unit = {
      if (!traversal.isInCurrentPath(element.id) || selfEncoded(element)) {
        val id = element.id
        traversal + id

        val sources = SourceMap(id, element)

        val obj = metaModel(element)

        createTypeNode(id, obj, Some(element))

        // workaround for lazy values in shape
        val modelFields = obj.fields ++ (obj match {
          case _: ShapeModel =>
            Seq(
              ShapeModel.CustomShapePropertyDefinitions,
              ShapeModel.CustomShapeProperties
            )
          case _ => Nil
        }).filter(options.renderField)

        element match {
          case e: ObjectNode if options.isValidation =>
            val url = Namespace.AmfValidation.base + "/properties"
            objectValue(id, url, Type.Int, Value(AmfScalar(e.propertyFields().size), Annotations()))
          case _ => // Nothing to do
        }

        modelFields.foreach { f =>
          emitStaticField(f, id, element, obj, sources)
        }
        createCustomExtensions(element)

        val sourceMapId = if (id.endsWith("/")) {
          id + "source-map"
        } else if (id.contains("#") || id.startsWith("null")) { // TODO: remove this null case
          id + "/source-map"
        } else {
          id + "#/source-map"
        }
        createSourcesNode(id, sources, sourceMapId)
      }
    }

    def emitStaticField(field: Field, id: String, element: AmfObject, obj: Obj, sources: SourceMap): Unit = {

      element.fields.entryJsonld(field) match {
        case Some(FieldEntry(f, v)) =>
          val url = f.value.iri()
          sources.property(url)(v)
          objectValue(id, url, f.`type`, v)
        case None => // Missing field
      }
    }

    private def createCustomExtensions(element: AmfObject): Unit = {
      val id                                   = element.id
      val customProperties: ListBuffer[String] = ListBuffer()

      // Collect element custom annotations
      element.fields.entry(DomainElementModel.CustomDomainProperties) foreach {
        case FieldEntry(_, v) =>
          v.value match {
            case AmfArray(values, _) =>
              values.foreach {
                case extension: DomainExtension =>
                  val uri = extension.definedBy.id
                  customProperties += uri
                  createCustomExtension(id, uri, extension, None)
              }
            case _ => // ignore
          }
      }

      // Collect element scalar fields custom annotations
      var count = 1
      element.fields.foreach {
        case (f, v) =>
          v.value.annotations
            .collect({ case e: DomainExtensionAnnotation => e })
            .foreach(e => {
              val extension = e.extension
              val uri       = s"${element.id}/scalar-valued/$count/${extension.name.value()}"
              customProperties += uri
              adoptTree(uri, extension.extension) // Fix ids
              createCustomExtension(id, uri, extension, Some(f))
              count += 1
            })
      }

      if (customProperties.nonEmpty) {
        customProperties.foreach { customPropertyId =>
          iri(id, DomainElementModel.CustomDomainProperties.value.iri(), customPropertyId)
        }
      }
    }

    private def createCustomExtension(subject: String,
                                      uri: String,
                                      extension: DomainExtension,
                                      field: Option[Field] = None): Unit = {
      rdfmodel.addTriple(subject, uri, extension.extension.id)
      rdfmodel.addTriple(uri, DomainExtensionModel.Name.value.iri(), extension.name.value(), None)
      field.foreach { f =>
        rdfmodel.addTriple(uri, DomainExtensionModel.Element.value.iri(), f.value.iri())
      }
      traverse(extension.extension)
    }

    def createSortedArray(subject: String, property: String, seq: Seq[AmfElement], element: Type): Unit = {
      val id = s"$subject/list"
      rdfmodel
        .addTriple(subject, property, id)
        .addTriple(id, (Namespace.Rdf + "type").iri(), (Namespace.Rdfs + "Seq").iri())
      seq.zipWithIndex.foreach {
        case (e, i) =>
          val memberTriple = (Namespace.Rdfs + s"_${i + 1}").iri()
          objectValue(id, memberTriple, element, Value(e, Annotations()))
      }
    }

    private def objectValue(subject: String, property: String, t: Type, v: Value): Unit = {
      t match {
        case t: DomainElement with Linkable if t.isLink =>
          link(subject, property, t)
        case _: Obj =>
          obj(subject, property, v.value.asInstanceOf[AmfObject])
        case Iri =>
          iri(subject, property, v.value.asInstanceOf[AmfScalar].toString)
        case EncodedIri =>
          safeIri(subject, property, v.value.asInstanceOf[AmfScalar].toString)
        case LiteralUri =>
          typedScalar(subject, property, v.value.asInstanceOf[AmfScalar].toString, DataType.AnyUri)
        case Str =>
          v.annotations.find(classOf[ScalarType]) match {
            case Some(annotation) =>
              typedScalar(subject, property, v.value.asInstanceOf[AmfScalar].toString, annotation.datatype)
            case None =>
              rdfmodel.addTriple(subject, property, v.value.asInstanceOf[AmfScalar].toString, None)
          }
        case Bool =>
          rdfmodel.addTriple(subject,
            property,
            v.value.asInstanceOf[AmfScalar].toString,
            Some(DataType.Boolean))
        case Type.Int =>
          emitIntLiteral(subject, property, v.value.asInstanceOf[AmfScalar].toString)
        case Type.Double =>
          // this will transform the value to double and will not emit @type TODO: ADD YType.Double
          // @TODO: see also in the Type.Any emitter
          rdfmodel.addTriple(subject,
            property,
            v.value.asInstanceOf[AmfScalar].toString,
            Some(DataType.Double))
        case Type.Float =>
          emitFloatLiteral(subject, property, v.value.asInstanceOf[AmfScalar].toString)
        case Type.DateTime =>
          val dateTime = v.value.asInstanceOf[AmfScalar].value.asInstanceOf[SimpleDateTime]
          typedScalar(subject, property, dateTime.toString, DataType.DateTime)
        case Type.Date =>
          val dateTime = v.value.asInstanceOf[AmfScalar].value.asInstanceOf[SimpleDateTime]
          if (dateTime.timeOfDay.isDefined || dateTime.zoneOffset.isDefined) {
            typedScalar(subject, property, dateTime.toString, DataType.DateTime)
          } else {
            typedScalar(subject, property, dateTime.toString, DataType.Date)
          }
        case a: SortedArray =>
          createSortedArray(subject, property, v.value.asInstanceOf[AmfArray].values, a.element)
        case a: Array =>
          v.value.asInstanceOf[AmfArray].values.foreach { e =>
            objectValue(subject, property, a.element, Value(e, Annotations()))
          }

        case Type.Any =>
          v.value.asInstanceOf[AmfScalar].value match {
            case bool: Boolean =>
              rdfmodel.addTriple(subject,
                property,
                v.value.asInstanceOf[AmfScalar].toString,
                Some(DataType.Boolean))
            case i: Int =>
              emitIntLiteral(subject, property, v.value.asInstanceOf[AmfScalar].toString)
            case f: Float =>
              emitFloatLiteral(subject, property, v.value.asInstanceOf[AmfScalar].toString)
            case d: Double =>
              rdfmodel.addTriple(subject,
                property,
                v.value.asInstanceOf[AmfScalar].toString,
                Some(DataType.Double))
            case _ =>
              v.annotations.find(classOf[ScalarType]) match {
                case Some(annotation) =>
                  typedScalar(subject, property, v.value.asInstanceOf[AmfScalar].toString, annotation.datatype)
                case None =>
                  rdfmodel.addTriple(subject, property, v.value.asInstanceOf[AmfScalar].toString, None)
              }
          }
      }
    }

    def emitIntLiteral(subject: String, property: String, v: String): Unit = {
      try {
        rdfmodel.addTriple(subject, property, v.toInt.toString, Some(DataType.Long))
      } catch {
        case _: NumberFormatException =>
          rdfmodel.addTriple(subject, property, v, None)
      }
    }

    def emitFloatLiteral(subject: String, property: String, v: String): Unit = {
      try {
        rdfmodel.addTriple(subject, property, v.toDouble.toString, Some(DataType.Double))
      } catch {
        case _: NumberFormatException =>
          rdfmodel.addTriple(subject, property, v, None)
      }
    }

    private def obj(subject: String, property: String, element: AmfObject): Unit = {
      rdfmodel.addTriple(subject, property, element.id)
      traverse(element)
    }

    private def link(subject: String, property: String, elementWithLink: DomainElement with Linkable): Unit = {
      // before emitting, we remove the link target to avoid loops and set
      // the fresh value for link-id
      val savedLinkTarget = elementWithLink.linkTarget
      elementWithLink.linkTarget.foreach { target =>
        elementWithLink.set(LinkableElementModel.TargetId, target.id)
        elementWithLink.fields.removeField(LinkableElementModel.Target)
      }

      // add the liking triple
      rdfmodel.addTriple(subject, property, elementWithLink.id)
      // recursion on the object
      traverse(elementWithLink)

      // we reset the link target after emitting
      savedLinkTarget.foreach { target =>
        elementWithLink.fields.setWithoutId(LinkableElementModel.Target, target)
      }
    }

    private def iri(subject: String, property: String, content: String): Unit = {
      // Last update, we assume that the iris are valid and han been encoded. Other option could be use the previous custom lcoal object URLEncoder but not search for %.
      // That can be a problem, because some chars could not being encoded
      rdfmodel.addTriple(subject, property, content)
    }

    private def safeIri(subject: String, property: String, content: String): Unit = {
      rdfmodel.addTriple(subject, property, content)
    }

    private def typedScalar(subject: String, property: String, content: String, dataType: String): Unit = {
      dataType match {
        case _ if dataType == DataType.Integer =>
          rdfmodel.addTriple(subject, property, content, Some(DataType.Long))
        case _ => rdfmodel.addTriple(subject, property, content, Some(dataType))
      }
    }

    private def createTypeNode(id: String, obj: Obj, maybeElement: Option[AmfObject] = None): Unit = {
      val allTypes = obj.`type`.map(_.iri())
      allTypes.foreach { t =>
        //if (t != "http://a.ml/vocabularies/document#DomainElement" && t != "http://www.w3.org/ns/shacl#Shape" && t != "http://a.ml/vocabularies/shapes#Shape")
        rdfmodel.addTriple(id, (Namespace.Rdf + "type").iri(), t)
      }
    }

    private def createSourcesNode(id: String, sources: SourceMap, sourceMapId: String): Unit = {
      if (options.isWithSourceMaps && sources.nonEmpty) {
        rdfmodel.addTriple(id, DomainElementModel.Sources.value.iri(), sourceMapId)
        createTypeNode(sourceMapId, SourceMapModel, None)
        createAnnotationNodes(sourceMapId, sources)
      }
    }

    private def createAnnotationNodes(id: String, sources: SourceMap): Unit = {
      sources.annotations.zipWithIndex.foreach({
        case ((a, values), i) =>
          values.zipWithIndex.foreach {
            case ((iri, v), j) =>
              val valueNodeId = s"${id}_${i}_$j"
              rdfmodel.addTriple(id, ValueType(Namespace.SourceMaps, a).iri(), valueNodeId)
              rdfmodel.addTriple(valueNodeId, SourceMapModel.Element.value.iri(), iri)
              rdfmodel.addTriple(valueNodeId, SourceMapModel.Value.value.iri(), v, None)
          }
      })
    }
  }
}
