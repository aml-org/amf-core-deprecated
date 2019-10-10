package amf.plugins.document.graph.emitter

import amf.core.annotations._
import amf.core.emitter.RenderOptions
import amf.core.metamodel.Type.{Any, Array, Bool, EncodedIri, Iri, LiteralUri, SortedArray, Str}
import amf.core.metamodel._
import amf.core.metamodel.document.{ModuleModel, SourceMapModel}
import amf.core.metamodel.domain.extensions.DomainExtensionModel
import amf.core.metamodel.domain.{DomainElementModel, ExternalSourceElementModel, LinkableElementModel, ShapeModel}
import amf.core.model.DataType
import amf.core.model.document.{BaseUnit, SourceMap}
import amf.core.model.domain.DataNodeOps.adoptTree
import amf.core.model.domain._
import amf.core.model.domain.extensions.DomainExtension
import amf.core.parser.{Annotations, FieldEntry, Value}
import amf.core.utils._
import amf.core.vocabulary.{Namespace, ValueType}
import org.mulesoft.common.time.SimpleDateTime
import org.yaml.builder.DocBuilder
import org.yaml.builder.DocBuilder.{Entry, Part, SType, Scalar}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object FlattenedJsonLdEmitter {

  def emit[T](unit: BaseUnit, builder: DocBuilder[T], renderOptions: RenderOptions = RenderOptions()): Boolean = {
    implicit val ctx: EmissionContext = FlattenedEmissionContext(unit, renderOptions)
    new FlattenedJsonLdEmitter[T](builder, renderOptions).root(unit)
    true
  }
}

case class EmissionFn[T](fn: Part[T] => Unit)

case class EmissionQueue[T]()(implicit ctx: EmissionContext) {
  private val declarationsQueue: mutable.Queue[Either[AmfObject, EmissionFn[T]]]    = mutable.Queue.empty
  private val nonDeclarationsQueue: mutable.Queue[Either[AmfObject, EmissionFn[T]]] = mutable.Queue.empty

  def enqueue(amfObject: AmfObject): Unit = enqueue(Left(amfObject))

  def enqueue(emissionFn: EmissionFn[T]): Unit = enqueue(Right(emissionFn))

  def enqueue(queueElement: Either[AmfObject, EmissionFn[T]]): Unit = {
    if (ctx.emittingDeclarations) {
      declarationsQueue.enqueue(queueElement)
    } else {
      nonDeclarationsQueue.enqueue(queueElement)
    }
  }

  def hasPendingDeclarations: Boolean = declarationsQueue.nonEmpty

  def hasPendingNonDeclarations: Boolean = nonDeclarationsQueue.nonEmpty

  def nextDeclaration(): Either[AmfObject, EmissionFn[T]] = declarationsQueue.dequeue()

  def nextNonDeclaration(): Either[AmfObject, EmissionFn[T]] = nonDeclarationsQueue.dequeue()

  def exists(p: Either[AmfObject, EmissionFn[T]] => Boolean): Boolean = {
    if (ctx.emittingDeclarations) {
      declarationsQueue.exists(p)
    } else {
      nonDeclarationsQueue.exists(p)
    }
  }

}

class FlattenedJsonLdEmitter[T](val builder: DocBuilder[T], val options: RenderOptions)(implicit ctx: EmissionContext)
    extends MetaModelTypeMapping {

  val pending: EmissionQueue[T]        = EmissionQueue()
  val seenIds: mutable.HashSet[String] = mutable.HashSet.empty
  var root: Part[T]                    = _

  def root(unit: BaseUnit): Unit = {
    val entry: Option[FieldEntry]      = unit.fields.entry(ModuleModel.Declares)
    val elements: Iterable[AmfElement] = entry.map(_.value.value.asInstanceOf[AmfArray].values).getOrElse(Nil)
    ctx ++ elements
    unit.fields.removeField(ModuleModel.Declares)

    builder.obj { ob =>
      ob.entry(
        "@graph",
        _.list { rootBuilder =>
          root = rootBuilder

          expandRoot(unit)

          ctx.emittingDeclarations(true)
          while (pending.hasPendingDeclarations) {
            pending.nextDeclaration() match {
              case Left(obj)       => expandObject(obj)
              case Right(emission) => emission.fn(rootBuilder)
            }
          }
          ctx.emittingDeclarations(false)

          while (pending.hasPendingNonDeclarations) {
            pending.nextNonDeclaration() match {
              case Left(obj)       => expandObject(obj)
              case Right(emission) => emission.fn(rootBuilder)
            }
          }
          entry.foreach(e => unit.fields.setWithoutId(ModuleModel.Declares, e.array))
        }
      )
      ctx.emitContext(ob)
    }
  }

  private def emitDeclarations(b: Entry[T], id: String, sources: SourceMap): Unit = {
    if (ctx.declared.nonEmpty) {
      val v   = Value(AmfArray(ctx.declared), Annotations())
      val f   = ModuleModel.Declares
      val url = ctx.emitIri(f.value.iri())
      ctx.emittingDeclarations(true)
      b.entry(
        url,
        value(f.`type`, v, id, sources.property(url), _)
      )
    }
    ctx.emittingDeclarations(false)
  }

  def expandRoot(unit: BaseUnit): Unit = {
    val id = unit.id

    root.obj { b =>
      createIdNode(b, id)
      seenIds += id
      emitDeclarations(b, unit.id, SourceMap(unit.id, unit))

      seenIds += id
      val sources = SourceMap(id, unit)
      val obj     = metaModel(unit)
      traverseMetaModel(id, unit, sources, obj, b)
      createCustomExtensions(unit, b)

      val sourceMapId = if (id.endsWith("/")) {
        id + "source-map"
      } else if (id.contains("#") || id.startsWith("null")) {
        id + "/source-map"
      } else {
        id + "#/source-map"
      }
      createSourcesNode(sourceMapId, sources, b)

    }

  }

  def expandObject(amfObject: AmfObject): Unit = {
    val id = amfObject.id

    root.obj { b =>
      createIdNode(b, id)

      seenIds += id
      val sources = SourceMap(id, amfObject)

      val obj = metaModel(amfObject)

      traverseMetaModel(id, amfObject, sources, obj, b)

      createCustomExtensions(amfObject, b)

      val sourceMapId = if (id.endsWith("/")) {
        id + "source-map"
      } else if (id.contains("#") || id.startsWith("null")) {
        id + "/source-map"
      } else {
        id + "#/source-map"
      }
      createSourcesNode(sourceMapId, sources, b)

    }
  }

  def traverseMetaModel(id: String, element: AmfObject, sources: SourceMap, obj: Obj, b: Entry[T]): Unit = {
    createTypeNode(b, obj, Some(element))

    val objFields = element match {
      case e: ExternalSourceElement if e.isLinkToSource => obj.fields.filter(f => f != ExternalSourceElementModel.Raw)
      case _                                            => obj.fields
    }
    // workaround for lazy values in shape
    val modelFields = objFields ++ (obj match {
      case _: ShapeModel =>
        Seq(
          ShapeModel.CustomShapePropertyDefinitions,
          ShapeModel.CustomShapeProperties
        )
      case _ => Nil
    })

    // no longer necessary?
    element match {
      case e: ObjectNode if options.isValidation =>
        val url = Namespace.AmfValidation.base + "/properties"
        b.entry(
          url,
          value(Type.Int, Value(AmfScalar(e.propertyFields().size), Annotations()), id, _ => {}, _)
        )
      case _ => // Nothing to do
    }

    modelFields.foreach { f =>
      emitStaticField(f, element, id, sources, b)
    }

  }

  private def emitStaticField(field: Field, element: AmfObject, id: String, sources: SourceMap, b: Entry[T]): Unit = {
    element.fields.entryJsonld(field) match {
      case Some(FieldEntry(f, v)) =>
        val url = ctx.emitIri(f.value.iri())
        b.entry(
          url,
          value(f.`type`, v, id, sources.property(url), _)
        )
      case None => // Missing field
    }
  }

  private def createCustomExtensions(element: AmfObject, b: Entry[T]): Unit = {
    val customProperties: ListBuffer[String] = ListBuffer()

    // Collect element custom annotations
    element.fields.entry(DomainElementModel.CustomDomainProperties) foreach {
      case FieldEntry(_, v) =>
        v.value match {
          case AmfArray(values, _) =>
            values
              .sortBy(_.asInstanceOf[DomainExtension].id)
              .foreach {
                case extension: DomainExtension =>
                  val uri = extension.definedBy.id
                  customProperties += uri
                  createCustomExtension(b, uri, extension, None)
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
          .sortBy(_.extension.id)
          .foreach(e => {
            val extension = e.extension
            val uri       = s"${element.id}/scalar-valued/$count/${extension.name.value()}"
            customProperties += uri
            adoptTree(uri, extension.extension) // Fix ids
            createCustomExtension(b, uri, extension, Some(f))
            count += 1
          })
    }

    if (customProperties.nonEmpty)
      b.entry(
        ctx.emitIri(DomainElementModel.CustomDomainProperties.value.iri()),
        _.list { b =>
          customProperties.foreach(iri(b, _, inArray = true))
        }
      )
  }

  private def createCustomExtension(b: Entry[T],
                                    uri: String,
                                    extension: DomainExtension,
                                    field: Option[Field] = None): Unit = {
    b.entry(
      uri,
      _.obj { b =>
        b.entry(
          ctx.emitIri(DomainExtensionModel.Name.value.iri()),
          emitScalar(_, extension.name.value())
        )
        field.foreach(
          f =>
            b.entry(
              ctx.emitIri(DomainExtensionModel.Element.value.iri()),
              emitScalar(_, f.value.iri())
          ))

        expandIfNeeded(extension.extension)
      }
    )
  }

  private def expandIfNeeded(obj: AmfObject): Unit = {
    val id = obj.id
    val needToExpand = !seenIds.contains(id) && !pending.exists {
      case Left(amfObject) => amfObject.id == id
      case Right(_)        => false
    }
    if (needToExpand) {
      pending.enqueue(Left(obj))
    }
  }

  def createSortedArray(b: Part[T],
                        seq: Seq[AmfElement],
                        parent: String,
                        element: Type,
                        sources: Value => Unit,
                        v: Option[Value] = None): Unit = {
    b.list {
      _.obj { b =>
        val id = s"$parent/list"
        createIdNode(b, id)
        b.entry("@type", ctx.emitIri((Namespace.Rdfs + "Seq").iri()))
        seq.zipWithIndex.foreach {
          case (e, i) =>
            b.entry(
              ctx.emitIri((Namespace.Rdfs + s"_${i + 1}").iri()), { b =>
                b.list {
                  b =>
                    element match {
                      case _: Obj =>
                        e match {
                          case elementInArray: DomainElement with Linkable if elementInArray.isLink =>
                            link(b, elementInArray, inArray = true)
                          case elementInArray: AmfObject =>
                            obj(b, elementInArray, inArray = true)
                        }
                      case Str =>
                        scalar(b, e, SType.Str)

                      case EncodedIri =>
                        safeIri(b, e.asInstanceOf[AmfScalar].toString, inArray = true)

                      case Iri =>
                        iri(b, e.asInstanceOf[AmfScalar].toString, inArray = true)

                      case Any =>
                        val scalarElement = e.asInstanceOf[AmfScalar]
                        scalarElement.value match {
                          case bool: Boolean =>
                            typedScalar(b, bool.toString, DataType.Boolean, inArray = true)
                          case str: String =>
                            typedScalar(b, str.toString, DataType.String, inArray = true)
                          case i: Int =>
                            typedScalar(b, i.toString, DataType.Integer, inArray = true)
                          case f: Float =>
                            typedScalar(b, f.toString, DataType.Float, inArray = true)
                          case d: Double =>
                            typedScalar(b, d.toString, DataType.Double, inArray = true)
                          case other => scalar(b, other.toString)
                        }
                    }
                }
              }
            )
        }
        v.foreach(sources)
      }
    }
  }

  private def shouldReconstructInheritance(v: Value, parent: String) = {
    val valueIsDeclared  = ctx.isDeclared(v.value)
    val parentIsDeclared = ctx.isDeclared(parent)
    !ctx.emittingDeclarations || (valueIsDeclared && parentIsDeclared)
  }

  private def isResolvedInheritance(v: Value) = v.value.annotations.contains(classOf[ResolvedInheritance])

  private def value(t: Type, v: Value, parent: String, sources: Value => Unit, b: Part[T]): Unit = {
    t match {
      case _: ShapeModel if isResolvedInheritance(v) && shouldReconstructInheritance(v, parent) =>
        extractToLink(v.value.asInstanceOf[Shape], b)
      case t: DomainElement with Linkable if t.isLink =>
        link(b, t)
        sources(v)
      case _: Obj =>
        obj(b, v.value.asInstanceOf[AmfObject])
        sources(v)
      case Iri =>
        iri(b, v.value.asInstanceOf[AmfScalar].toString)
        sources(v)
      case EncodedIri =>
        safeIri(b, v.value.asInstanceOf[AmfScalar].toString)
        sources(v)
      case LiteralUri =>
        typedScalar(b, v.value.asInstanceOf[AmfScalar].toString, DataType.AnyUri)
        sources(v)
      case Str =>
        emitScalar(b, v.value)
        sources(v)
      case Bool =>
        emitScalar(b, v.value, SType.Bool)
        sources(v)
      case Type.Int =>
        emitScalar(b, v.value, SType.Int)
        sources(v)
      case Type.Double =>
        // this will transform the value to double and will not emit @type TODO: ADD YType.Double
        emitScalar(b, v.value, SType.Float)
        sources(v)
      case Type.Float =>
        emitScalar(b, v.value, SType.Float)
        sources(v)
      case Type.DateTime =>
        val dateTime = v.value.asInstanceOf[AmfScalar].value.asInstanceOf[SimpleDateTime]
        typedScalar(b, emitDateFormat(dateTime), DataType.DateTime)
        sources(v)
      case Type.Date =>
        val maybeDateTime = v.value.asInstanceOf[AmfScalar].value match {
          case dt: SimpleDateTime => Some(dt)
          case other              => SimpleDateTime.parse(other.toString).toOption
        }
        maybeDateTime match {
          case Some(dateTime) =>
            if (dateTime.timeOfDay.isDefined || dateTime.zoneOffset.isDefined) {
              typedScalar(b, emitDateFormat(dateTime), DataType.DateTime)
            } else {
              typedScalar(b, f"${dateTime.year}%04d-${dateTime.month}%02d-${dateTime.day}%02d", DataType.Date)

            }
          case _ =>
            emitScalar(b, v.value)
        }
        sources(v)
      case a: SortedArray =>
        createSortedArray(b, v.value.asInstanceOf[AmfArray].values, parent, a.element, sources, Some(v))
      case a: Array =>
        b.list { b =>
          val seq = v.value.asInstanceOf[AmfArray]
          sources(v)
          a.element match {
            case _: Obj =>
              seq.values.asInstanceOf[Seq[AmfObject]].foreach {
                case v @ (_: Shape)
                    if v.annotations
                      .contains(classOf[ResolvedInheritance]) && ((!ctx.emittingDeclarations) || (ctx.emittingDeclarations && ctx
                      .isDeclared(v) && ctx.isDeclared(parent))) =>
                  extractToLink(v.asInstanceOf[Shape], b)
                case elementInArray: DomainElement with Linkable if elementInArray.isLink =>
                  link(b, elementInArray, inArray = true)
                case elementInArray =>
                  obj(b, elementInArray, inArray = true)
              }
            case Str =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach { e =>
                scalar(b, e.toString)
              }
            case EncodedIri =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach(e => safeIri(b, e.toString, inArray = true))
            case Iri =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach(e => iri(b, e.toString, inArray = true))
            case LiteralUri =>
              typedScalar(b, v.value.asInstanceOf[AmfScalar].toString, DataType.AnyUri, inArray = true)
            case Type.Int =>
              seq.values
                .asInstanceOf[Seq[AmfScalar]]
                .foreach(e => scalar(b, e.value.toString, SType.Int))
            case Type.Float =>
              seq.values
                .asInstanceOf[Seq[AmfScalar]]
                .foreach(e => scalar(b, e.value.toString, SType.Float))
            case Bool =>
              seq.values
                .asInstanceOf[Seq[AmfScalar]]
                .foreach(e => scalar(b, e.value.toString, SType.Bool))
            case Type.DateTime =>
              seq.values
                .asInstanceOf[Seq[AmfScalar]]
                .foreach { e =>
                  val dateTime = e.value.asInstanceOf[SimpleDateTime]
                  typedScalar(b, emitDateFormat(dateTime), DataType.DateTime)
                }
            case Type.Date =>
              seq.values
                .asInstanceOf[Seq[AmfScalar]]
                .foreach { e =>
                  val dateTime = e.value.asInstanceOf[SimpleDateTime]
                  emitSimpleDateTime(b, dateTime, inArray = false)
                }
            case Any =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach { scalarElement =>
                scalarElement.value match {
                  case bool: Boolean =>
                    typedScalar(b, bool.toString, DataType.Boolean, inArray = true)
                  case i: Int              => typedScalar(b, i.toString, DataType.Integer, inArray = true)
                  case f: Float            => typedScalar(b, f.toString, DataType.Float, inArray = true)
                  case d: Double           => typedScalar(b, d.toString, DataType.Double, inArray = true)
                  case sdt: SimpleDateTime => emitSimpleDateTime(b, sdt)
                  case other               => scalar(b, other.toString)
                }
              }
            case _ => seq.values.asInstanceOf[Seq[AmfScalar]].foreach(e => iri(b, e.toString, inArray = true))
          }
        }

      case Any if v.value.isInstanceOf[AmfScalar] =>
        v.value.asInstanceOf[AmfScalar].value match {
          case bool: Boolean       => typedScalar(b, bool.toString, DataType.Boolean, inArray = true)
          case i: Int              => typedScalar(b, i.toString, DataType.Integer, inArray = true)
          case f: Float            => typedScalar(b, f.toString, DataType.Float, inArray = true)
          case d: Double           => typedScalar(b, d.toString, DataType.Double, inArray = true)
          case sdt: SimpleDateTime => emitSimpleDateTime(b, sdt)
          case other               => scalar(b, other.toString)
        }
    }
  }

  private def emitSimpleDateTime(b: Part[T], dateTime: SimpleDateTime, inArray: Boolean = true): Unit = {
    if (dateTime.timeOfDay.isDefined || dateTime.zoneOffset.isDefined) {
      typedScalar(b, emitDateFormat(dateTime), DataType.DateTime, inArray)
    } else {
      typedScalar(b, f"${dateTime.year}%04d-${dateTime.month}%02d-${dateTime.day}%02d", DataType.Date)
    }
  }

  private def obj(b: Part[T], obj: AmfObject, inArray: Boolean = false): Unit = {
    def emit(b: Part[T]): Unit = {
      b.obj(createIdNode(_, obj.id))
      expandIfNeeded(obj)
    }
    emit(b)
  }

  private def extractToLink(shape: Shape, b: Part[T]): Unit = {
    if (!ctx.isDeclared(shape)) {
      ctx + shape
      shape.name.option() match {
        case Some("schema") | Some("type") | None => shape.withName(ctx.nextTypeName).annotations += InlineElement()
        case _ if !shape.annotations.contains(classOf[DeclaredElement]) =>
          shape.withName(ctx.nextTypeName).annotations += InlineElement() // to catch media type named cases.
        case _ => // ignore
      }
    }
    val linked = shape match {
      // if it is recursive we force the conversion into a linked shape
      case rec: RecursiveShape =>
        RecursiveShape()
          .withId(rec.id + "/linked")
          .withLinkTarget(rec)
          .withLinkLabel(shape.name.value())
      // no recursive we just generate the linked shape
      case _ =>
        shape.link[Shape](shape.name.value())
    }

    link(b, linked)
  }

  private def link(b: Part[T], elementWithLink: DomainElement with Linkable, inArray: Boolean = false): Unit = {
    def emit(b: Part[T]): Unit = {
      // before emitting, we remove the link target to avoid loops and set
      // the fresh value for link-id
      val savedLinkTarget = elementWithLink.linkTarget
      elementWithLink.linkTarget.foreach { target =>
        elementWithLink.set(LinkableElementModel.TargetId, target.id)
        elementWithLink.fields.removeField(LinkableElementModel.Target)
      }

      b.obj { o =>
        createIdNode(o, elementWithLink.id)
        expandIfNeeded(elementWithLink)
      }
      // we reset the link target after emitting
      savedLinkTarget.foreach { target =>
        elementWithLink.fields.setWithoutId(LinkableElementModel.Target, target)
      }
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def iri(b: Part[T], content: String, inArray: Boolean = false): Unit = {
    // Last update, we assume that the iris are valid and han been encoded. Other option could be use the previous custom lcoal object URLEncoder but not search for %.
    // That can be a problem, because some chars could not being encoded
    def emit(b: Part[T]): Unit = {
      b.obj(_.entry("@id", raw(_, ctx.emitId(content))))
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def safeIri(b: Part[T], content: String, inArray: Boolean = false): Unit = {
    def emit(b: Part[T]): Unit = {
      b.obj(_.entry("@id", raw(_, ctx.emitId(content))))
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def typedScalar(b: Part[T], content: String, dataType: String, inArray: Boolean = false): Unit = {
    def emit(b: Part[T]): Unit = b.obj { m =>
      m.entry("@value", raw(_, content))
      m.entry("@type", raw(_, ctx.emitIri(dataType)))
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def createIdNode(b: Entry[T], id: String): Unit = b.entry(
    "@id",
    raw(_, ctx.emitId(id))
  )

  private def createTypeNode(b: Entry[T], obj: Obj, maybeElement: Option[AmfObject] = None): Unit = {
    b.entry(
      "@type",
      _.list { b =>
        val allTypes = obj.`type`.map(_.iri())
        allTypes.distinct.foreach(t => raw(b, ctx.emitIri(t)))
      }
    )
  }

  private def raw(b: Part[T], content: String): Unit =
    b += content

  private def createSourcesNode(id: String, sources: SourceMap, b: Entry[T]): Unit = {
    if (options.isWithSourceMaps && sources.nonEmpty) {
      if (options.isWithRawSourceMaps) {
        b.entry(
          "smaps",
          _.obj { b =>
            createAnnotationNodes(b, sources.annotations)
            createAnnotationNodes(b, sources.eternals)
          }
        )
      } else {
        b.entry(
          ctx.emitIri(DomainElementModel.Sources.value.iri()),
          _.list {
            _.obj { b =>
              // TODO: Maybe this should be emitted in root
              createIdNode(b, id)
              if (!seenIds.contains(id)) {
                val fn = EmissionFn((part: Part[T]) => {
                  part.obj { rb =>
                    createIdNode(rb, id)
                    createTypeNode(rb, SourceMapModel, None)
                    createAnnotationNodes(rb, sources.annotations)
                    createAnnotationNodes(rb, sources.eternals)
                  }
                })
                pending.enqueue(Right(fn))
              }
            }
          }
        )
      }
    } else {
      createEternalsAnnotationsNodes(id, options, b, sources)
    }
  }

  private def createEternalsAnnotationsNodes(id: String,
                                             options: RenderOptions,
                                             b: Entry[T],
                                             sources: SourceMap): Unit = {
    if (sources.eternals.nonEmpty)
      if (options.isWithRawSourceMaps) {
        b.entry(
          "smaps",
          _.obj { b =>
            createAnnotationNodes(b, sources.eternals)
          }
        )
      } else {
        b.entry(
          ctx.emitIri(DomainElementModel.Sources.value.iri()),
          _.list {
            _.obj { b =>
              createIdNode(b, id)
              if (!seenIds.contains(id)) {
                val fn = EmissionFn((part: Part[T]) => {
                  part.obj { rb =>
                    createIdNode(rb, id)
                    createTypeNode(rb, SourceMapModel, None)
                    createAnnotationNodes(rb, sources.eternals)
                  }
                })
                pending.enqueue(Right(fn))
              }
            }
          }
        )
      }
  }

  private def createAnnotationNodes(b: Entry[T],
                                    annotations: mutable.ListMap[String, mutable.ListMap[String, String]]): Unit = {
    annotations.foreach({
      case (a, values) =>
        if (ctx.options.isWithRawSourceMaps) {
          b.entry(
            a,
            _.obj { o =>
              values.foreach {
                case (iri, v) =>
                  o.entry(
                    ctx.emitId(ctx.emitIri(iri)),
                    raw(_, v)
                  )
              }
            }
          )
        } else {
          b.entry(
            ctx.emitIri(ValueType(Namespace.SourceMaps, a).iri()),
            _.list(b => values.foreach(createAnnotationValueNode(b, _)))
          )
        }
    })
  }

  private def createAnnotationValueNode(b: Part[T], tuple: (String, String)): Unit =
    tuple match {
      case (iri, v) =>
        b.obj { b =>
          b.entry(ctx.emitIri(SourceMapModel.Element.value.iri()), emitScalar(_, iri))
          b.entry(ctx.emitIri(SourceMapModel.Value.value.iri()), emitScalar(_, v))
        }
    }

  private def emitDateFormat(dateTime: SimpleDateTime) = dateTime.rfc3339

  private def scalar(b: Part[T], content: String, t: SType): Unit = b += Scalar(t, content)

  private def scalar(b: Part[T], content: String): Unit = scalar(b, content, SType.Str)

  private def scalar(b: Part[T], content: AmfElement, t: SType): Unit =
    scalar(b, content.asInstanceOf[AmfScalar].value.toString, t)

  private def emitScalar(b: Part[T], content: String, t: SType): Unit = scalar(b, content, t)
  private def emitScalar(b: Part[T], content: String): Unit           = emitScalar(b, content, SType.Str)

  private def emitScalar(b: Part[T], content: AmfElement, t: SType): Unit = scalar(b, content, t)
  private def emitScalar(b: Part[T], content: AmfElement): Unit           = emitScalar(b, content, SType.Str)

}
