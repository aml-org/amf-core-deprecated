package amf.plugins.document.graph.parser
import amf.core.annotations.DomainExtensionAnnotation
import amf.core.metamodel.Type.{Array, Bool, Iri, LiteralUri, RegExp, SortedArray, Str}
import amf.core.metamodel.document.{BaseUnitModel, DocumentModel}
import amf.core.metamodel.domain.extensions.DomainExtensionModel
import amf.core.metamodel.domain.{DomainElementModel, ExternalSourceElementModel, LinkableElementModel, ShapeModel}
import amf.core.metamodel.{Field, ModelDefaultBuilder, Obj, Type}
import amf.core.model.DataType
import amf.core.model.document._
import amf.core.model.domain._
import amf.core.model.domain.extensions.{CustomDomainProperty, DomainExtension}
import amf.core.parser._
import amf.core.registries.AMFDomainRegistry
import amf.core.remote.Platform
import amf.core.unsafe.TrunkPlatform
import amf.core.vocabulary.Namespace
import amf.plugins.features.validation.CoreValidations.{NodeNotFound, NotLinkable, UnableToParseDocument, UnableToParseNode}
import org.mulesoft.common.time.SimpleDateTime
import org.yaml.model._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

class FlattenedGraphParser(platform: Platform)(implicit val ctx: GraphParserContext)
    extends GraphParser
    with GraphParserHelpers {

  override def canParse(document: SyamlParsedDocument): Boolean = FlattenedGraphParser.canParse(document)

  override def parse(document: YDocument, location: String): BaseUnit = {
    val parser = Parser(Map())
    parser.parse(document, location)
  }

  case class Parser(var nodes: Map[String, AmfElement]) {
    private val unresolvedReferences = mutable.Map[String, Seq[DomainElement]]()
    private val unresolvedExtReferencesMap =
      mutable.Map[String, ExternalSourceElement]()

    private val referencesMap = mutable.Map[String, DomainElement]()
    private val cache = mutable.Map[String, AmfObject]()

    def parse(document: YDocument, location: String): BaseUnit = {
      var result: Option[AmfObject] = None
      document.node.value match {
        case documentMap: YMap =>
          documentMap.key("@context").foreach(e => parseCompactUris(e.value))
          documentMap.key("@graph").foreach { graphEntry =>
            graphEntry.value.to[YSequence] match {
              case Right(rootNodes) =>
                val rootGraphNodeIterator = rootNodes.nodes.reverseIterator
                while (rootGraphNodeIterator.hasNext){
                  val map = rootGraphNodeIterator.next().as[YMap]
                  val maybeObject = parse(map)
                  val rawIdOption = retrieveId(map, ctx).map(transformIdFromContext)
                  (rawIdOption, maybeObject) match {
                    case _ @ (Some(id), Some(amfObject)) =>
                      cache.put(id, amfObject)
                      result = Some(amfObject)
                    case _ =>
                      val location = s"${map.location.sourceName}:${map.location.lineFrom}.${map.location.columnFrom}"
                      ctx.violation(UnableToParseDocument, "", s"Cannot parse graph node at $location")
                  }

                }
              case _ =>
                ctx.violation(UnableToParseDocument, "", "Error parsing root JSON-LD node")
            }
          }
        case _ =>
          ctx.violation(UnableToParseDocument, "", "Error parsing root JSON-LD node")
      }
      result match {
        case _ @ Some(b: BaseUnit) => b.withLocation(location)
        case _ =>
          ctx.violation(UnableToParseDocument, "", "Error parsing root JSON-LD node")
          Document() // TODO returning empty document on failure
      }
    }

    private def parseCompactUris(node: YNode): Unit = {
      node.tagType match {
        case YType.Map =>
          val m: YMap = node.as[YMap]
          ctx.compactUris ++= m.entries.flatMap(parseKeyValue).toMap
          ctx.baseId = ctx.compactUris.find { case (key, value) => key == "@base" }.map { case (_, value) => value }
        case _ =>
      }
    }

    private def parseKeyValue(entry: YMapEntry): Option[(String, String)] = {
      (entry.key.tagType, entry.value.tagType) match {
        case (YType.Str, YType.Str) =>
          Some(entry.key.as[YScalar].text -> entry.value.as[YScalar].text)
        case _ => None
      }
    }

    private def retrieveType(id: String, map: YMap): Option[Obj] = {
      val stringTypes = ts(map, id)
      stringTypes.find(findType(_).isDefined) match {
        case Some(t) => findType(t)
        case None =>
          ctx.violation(UnableToParseNode, id, s"Error parsing JSON-LD node, unknown @types $stringTypes", map)
          None
      }
    }

    private def parseList(id: String, listElement: Type, node: YMap): Seq[AmfElement] = {
      val buffer = ListBuffer[YNode]()
      node.entries.sortBy(_.key.as[String]).foreach { entry =>
        if (entry.key.as[String].startsWith(compactUriFromContext((Namespace.Rdfs + "_").iri()))) {
          buffer += entry.value.as[Seq[YNode]].head
        }
      }
      buffer.flatMap { n =>
        listElement match {
          case _: Obj => parse(n.as[YMap])
          case _ =>
            try { Some(str(value(listElement, n))) } catch {
              case _: Exception => None
            }
        }
      }
    }

    private def parse(map: YMap): Option[AmfObject] = { // todo fix uses
      if (isJsonLdLink(map)) {
        retrieveId(map, ctx)
          .map(transformIdFromContext)
          .flatMap(cache.get)
      } else {
        retrieveId(map, ctx)
          .flatMap(value => retrieveType(value, map).map(value2 => (value, value2)))
          .flatMap {
            case (id, model) =>
              val sources               = retrieveSources(id, map)
              val transformedId: String = transformIdFromContext(id)
              buildType(transformedId, map, model)(annotations(nodes, sources, transformedId)) match {
                case Some(builder) =>
                  val instance: AmfObject = builder
                  instance.withId(transformedId)

                  checkLinkables(instance)

                  // workaround for lazy values in shape
                  val modelFields = model match {
                    case shapeModel: ShapeModel =>
                      shapeModel.fields ++ Seq(
                        ShapeModel.CustomShapePropertyDefinitions,
                        ShapeModel.CustomShapeProperties
                      )
                    case _ => model.fields
                  }

                  modelFields.foreach(f => {
                    val k = compactUriFromContext(f.value.iri())
                    map.key(k) match {
                      case Some(entry) =>
                        traverse(instance, f, value(f.`type`, entry.value), sources, k)
                      case _ =>
                    }
                  })

                  // parsing custom extensions
                  instance match {
                    case l: DomainElement with Linkable =>
                      parseLinkableProperties(map, l)
                    case ex: ExternalDomainElement if unresolvedExtReferencesMap.get(ex.id).isDefined =>
                      unresolvedExtReferencesMap.get(ex.id).foreach { element =>
                        ex.raw
                          .option()
                          .foreach(element.set(ExternalSourceElementModel.Raw, _))
                      }
                    case obj: ObjectNode =>
                      parseObjectNodeProperties(obj, map, modelFields)

                    case _ => // ignore
                  }
                  instance match {
                    case elm: DomainElement => parseCustomProperties(map, elm)
                    case _                  => // ignore
                  }

                  nodes = nodes + (transformedId -> instance)
                  Some(instance)
                case _ => None
              }

          }
      }

    }

    private def checkLinkables(instance: AmfObject): Unit = {
      instance match {
        case link: DomainElement with Linkable =>
          referencesMap += (link.id -> link)
          unresolvedReferences.getOrElse(link.id, Nil).foreach {
            case unresolved: Linkable =>
              unresolved.withLinkTarget(link)
            case unresolved: LinkNode =>
              unresolved.withLinkedDomainElement(link)
            case _ =>
              ctx.violation(NotLinkable, instance.id, "Only linkable elements can be linked", instance.annotations)
          }
          unresolvedReferences.update(link.id, Nil)
        case ref: ExternalSourceElement =>
          unresolvedExtReferencesMap += (ref.referenceId.value -> ref) // process when parse the references node
        case _ => // ignore
      }
    }

    private def setLinkTarget(instance: DomainElement with Linkable, targetId: String) = {
      referencesMap.get(targetId) match {
        case Some(target) => instance.withLinkTarget(target)
        case None =>
          val unresolved: Seq[DomainElement] =
            unresolvedReferences.getOrElse(targetId, Nil)
          unresolvedReferences += (targetId -> (unresolved ++ Seq(instance)))
      }
    }

    private def parseLinkableProperties(map: YMap, instance: DomainElement with Linkable): Unit = {
      map
        .key(compactUriFromContext(LinkableElementModel.TargetId.value.iri()))
        .flatMap(entry => {
          retrieveId(entry.value.as[Seq[YMap]].head, ctx)
        })
        .foreach { targetId =>
          val transformedId = transformIdFromContext(targetId)
          setLinkTarget(instance, transformedId)
        }

      map
        .key(compactUriFromContext(LinkableElementModel.Label.value.iri()))
        .flatMap(entry => {
          entry.value
            .toOption[Seq[YNode]]
            .flatMap(nodes => nodes.head.toOption[YMap])
            .flatMap(map => map.key("@value"))
            .flatMap(_.value.toOption[YScalar].map(_.text))
        })
        .foreach(s => instance.withLinkLabel(s))
    }

    private def parseCustomProperties(map: YMap, instance: DomainElement): Unit = {
      val properties = map
        .key(compactUriFromContext(DomainElementModel.CustomDomainProperties.value.iri()))
        .map(_.value.as[Seq[YNode]].map(value(Iri, _).as[YScalar].text))
        .getOrElse(Nil)

      val extensions = properties
        .flatMap { uri =>
          map
            .key(compactUriFromContext(uri))
            .map(entry => {
              val extension = DomainExtension()
              val obj       = entry.value.as[YMap]

              parseScalarProperty(obj, DomainExtensionModel.Name)
                .map(extension.withName)
              parseScalarProperty(obj, DomainExtensionModel.Element)
                .map(extension.withElement)

              val definition = CustomDomainProperty()
              definition.id = uri
              extension.withDefinedBy(definition)

              parse(obj).collect({ case d: DataNode => d }).foreach { pn =>
                extension.withId(pn.id)
                extension.withExtension(pn)
              }

              val sources = retrieveSources(extension.id, map)
              extension.annotations ++= annotations(nodes, sources, extension.id)

              extension
            })
        }

      if (extensions.nonEmpty) {
        extensions.partition(_.isScalarExtension) match {
          case (scalars, objects) =>
            instance.withCustomDomainProperties(objects)
            applyScalarDomainProperties(instance, scalars)
        }
      }
    }

    private def applyScalarDomainProperties(instance: DomainElement, scalars: Seq[DomainExtension]): Unit = {
      scalars.foreach { e =>
        instance.fields
          .fieldsMeta()
          .find(f => e.element.is(f.value.iri()))
          .foreach(f => {
            instance.fields.entry(f).foreach {
              case FieldEntry(_, value) =>
                value.value.annotations += DomainExtensionAnnotation(e)
            }
          })
      }
    }

    private def parseObjectNodeProperties(obj: ObjectNode, map: YMap, fields: List[Field]): Unit = {
      map.entries.foreach { entry =>
        val uri = expandUriFromContext(entry.key.as[String])
        val v   = entry.value
        if (uri != "@type" && uri != "@id" && uri != DomainElementModel.Sources.value.iri() && uri != "smaps" &&
            uri != (Namespace.Document + "name")
              .iri() && !fields.exists(_.value.iri() == uri)) { // we do this to prevent parsing name of annotations
          v.as[Seq[YMap]]
            .headOption
            .flatMap(parse)
            .collect({ case d: amf.core.model.domain.DataNode => obj.addProperty(uri, d) })
        }
      }
    }

    private def isJsonLdLink(m: YMap): Boolean =  m.entries.size == 1 && m.key("@id").isDefined

    private def traverse(instance: AmfObject, f: Field, node: YNode, sources: SourceMap, key: String) = {
      f.`type` match {
        case _: Obj =>
          parse(node.as[YMap]).foreach(n => instance.set(f, n, annotations(nodes, sources, key)))
          instance
        case Str | RegExp | Iri | LiteralUri =>
          instance.set(f, str(node), annotations(nodes, sources, key))
        case Bool =>
          instance.set(f, bool(node), annotations(nodes, sources, key))
        case Type.Int =>
          instance.set(f, int(node), annotations(nodes, sources, key))
        case Type.Float =>
          instance.set(f, float(node), annotations(nodes, sources, key))
        case Type.Double =>
          instance.set(f, double(node), annotations(nodes, sources, key))
        case Type.DateTime =>
          instance.set(f, date(node), annotations(nodes, sources, key))
        case Type.Date =>
          instance.set(f, date(node), annotations(nodes, sources, key))
        case Type.Any =>
          instance.set(f, any(node), annotations(nodes, sources, key))
        case l: SortedArray =>
          instance.setArray(f, parseList(instance.id, l.element, node.as[YMap]), annotations(nodes, sources, key))
        case a: Array =>
          val items = node.as[Seq[YNode]]
          val values: Seq[AmfElement] = a.element match {
            case _: Obj    => items.flatMap(n => parse(n.as[YMap]))
            case Str | Iri => items.map(n => str(value(a.element, n)))
          }
          a.element match {
            case _: DomainElementModel if f == DocumentModel.Declares =>
              instance.setArrayWithoutId(f, values, annotations(nodes, sources, key))
            case _: BaseUnitModel =>
              instance.setArrayWithoutId(f, values, annotations(nodes, sources, key))
            case _ =>
              instance.setArray(f, values, annotations(nodes, sources, key))
          }
      }
    }
  }

  private def parseScalarProperty(definition: YMap, field: Field) =
    definition
      .key(compactUriFromContext(field.value.iri()))
      .map(entry => value(field.`type`, entry.value).as[YScalar].text)

  private def str(node: YNode) = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) => entry.value.as[YScalar].text
          case _           => node.as[YScalar].text
        }
      case _ => node.as[YScalar].text
    }
    AmfScalar(value)
  }

  private def bool(node: YNode) = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) => entry.value.as[YScalar].text.toBoolean
          case _           => node.as[YScalar].text.toBoolean
        }
      case _ => node.as[YScalar].text.toBoolean
    }
    AmfScalar(value)
  }

  private def int(node: YNode) = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) => entry.value.as[YScalar].text.toInt
          case _           => node.as[YScalar].text.toInt
        }
      case _ => node.as[YScalar].text.toInt
    }
    AmfScalar(value)
  }

  private def double(node: YNode) = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) => entry.value.as[YScalar].text.toDouble
          case _           => node.as[YScalar].text.toDouble
        }
      case _ => node.as[YScalar].text.toDouble
    }
    AmfScalar(value)
  }

  private def date(node: YNode) = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) =>
            SimpleDateTime.parse(entry.value.as[YScalar].text).right.get
          case _ => SimpleDateTime.parse(node.as[YScalar].text).right.get
        }
      case _ => SimpleDateTime.parse(node.as[YScalar].text).right.get
    }
    AmfScalar(value)
  }

  private val xsdString: String   = DataType.String
  private val xsdInteger: String  = DataType.Integer
  private val xsdFloat: String    = DataType.Float
  private val amlNumber: String   = DataType.Number
  private val xsdDouble: String   = DataType.Double
  private val xsdBoolean: String  = DataType.Boolean
  private val xsdDateTime: String = DataType.DateTime
  private val xsdDate: String     = DataType.Date

  private def any(node: YNode) = {
    node.tagType match {
      case YType.Map =>
        val nodeValue =
          node.as[YMap].entries.find(_.key.as[String] == "@value") match {
            case Some(entry) => entry.value.as[YScalar].text
            case _           => node.as[YScalar].text
          }
        node.as[YMap].entries.find(_.key.as[String] == "@type") match {
          case Some(typeEntry) =>
            val typeUri     = typeEntry.value.as[YScalar].text
            val expandedUri = expandUriFromContext(typeUri)
            expandedUri match {
              case s: String if s == xsdBoolean =>
                AmfScalar(nodeValue.toBoolean)
              case s: String if s == xsdInteger => AmfScalar(nodeValue.toInt)
              case s: String if s == xsdFloat   => AmfScalar(nodeValue.toFloat)
              case s: String if s == xsdDouble  => AmfScalar(nodeValue.toDouble)
              case s: String if s == xsdDateTime =>
                AmfScalar(SimpleDateTime.parse(nodeValue).right.get)
              case s: String if s == xsdDate =>
                AmfScalar(SimpleDateTime.parse(nodeValue).right.get)
              case _ => AmfScalar(nodeValue)
            }
          case _ => AmfScalar(nodeValue)
        }
      case _ => AmfScalar(node.as[YScalar].text)
    }
  }

  private def float(node: YNode) = {
    val value = node.tagType match {
      case YType.Map =>
        node.as[YMap].entries.find(_.key.as[String] == "@value") match {
          case Some(entry) =>
            entry.value.as[YScalar].text.toDouble
          case _ => node.as[YScalar].text.toDouble
        }
      case _ => node.as[YScalar].text.toDouble
    }
    AmfScalar(value)
  }

  private val types: Map[String, Obj] = Map.empty ++ AMFDomainRegistry.metadataRegistry

  private def findType(typeString: String): Option[Obj] = {
    types.get(expandUriFromContext(typeString)).orElse(AMFDomainRegistry.findType(typeString))
  }

  private def buildType(id: String, map: YMap, modelType: Obj): Annotations => Option[AmfObject] = {
    AMFDomainRegistry.metadataRegistry.get(modelType.`type`.head.iri()) match {
      case Some(modelType: ModelDefaultBuilder) =>
        (annotations: Annotations) =>
          val instance = modelType.modelInstance
          instance.annotations ++= annotations
          Some(instance)
      case _ =>
        AMFDomainRegistry.buildType(modelType) match {
          case Some(builder) =>
            (a: Annotations) =>
              Some(builder(a))
          case _ =>
            ctx.violation(NodeNotFound, id, s"Cannot find builder for node type $modelType", map)
            (_: Annotations) =>
              None
        }
    }
  }
}

object FlattenedGraphParser {
  def apply: FlattenedGraphParser = FlattenedGraphParser(TrunkPlatform(""))
  def apply(platform: Platform): FlattenedGraphParser =
    new FlattenedGraphParser(platform)(new GraphParserContext())

  def canParse(document: SyamlParsedDocument): Boolean = {
    document.document.node.value match {
      case m: YMap =>
        m.key("@graph").isDefined
      case _ => false
    }
  }
}
