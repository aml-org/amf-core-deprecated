package amf.plugins.document.graph.parser

import amf.core.annotations._
import amf.core.emitter.RenderOptions
import amf.core.metamodel.Type.{Any, Array, Bool, EncodedIri, Iri, LiteralUri, SortedArray, Str}
import amf.core.metamodel.document.{ModuleModel, SourceMapModel}
import amf.core.metamodel.domain.extensions.DomainExtensionModel
import amf.core.metamodel.domain.{DomainElementModel, ExternalSourceElementModel, LinkableElementModel, ShapeModel}
import amf.core.metamodel._
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

object JsonLdEmitter {

  def emit[T](unit: BaseUnit, builder: DocBuilder[T], renderOptions: RenderOptions = RenderOptions()): Boolean = {
    new JsonLdEmitter[T](builder, renderOptions).root(unit)
    true
  }
}

class JsonLdEmitter[T](val builder: DocBuilder[T], val options: RenderOptions) extends MetaModelTypeMapping {

  val cache: mutable.Map[String, T] = mutable.Map[String, T]()

  def root(unit: BaseUnit): Unit = {
    val ctx                            = EmissionContext(unit, options)
    val entry: Option[FieldEntry]      = unit.fields.entry(ModuleModel.Declares)
    val elements: Iterable[AmfElement] = entry.map(_.value.value.asInstanceOf[AmfArray].values).getOrElse(Nil)
    ctx ++ elements
    unit.fields.removeField(ModuleModel.Declares)

    builder.list {
      _.obj { eb =>
        traverse(unit, eb, ctx)
        emitDeclarations(eb, unit.id, SourceMap(unit.id, unit), ctx)
        entry.foreach(e => unit.fields.setWithoutId(ModuleModel.Declares, e.array))
        ctx.emitContext(eb)
      }
    }
  }

  private def emitDeclarations(b: Entry[T], id: String, sources: SourceMap, ctx: EmissionContext): Unit = {
    if (ctx.declared.nonEmpty) {
      val v   = Value(AmfArray(ctx.declared), Annotations())
      val f   = ModuleModel.Declares
      val url = ctx.emitIri(f.value.iri())
      b.entry(
        url,
        value(f.`type`, v, id, sources.property(url), _, ctx.declares(true))
      )
    }
    ctx.declares(false)
  }

  def traverse(element: AmfObject, b: Entry[T], ctx: EmissionContext): Unit = {
    val id = element.id

    createIdNode(b, id, ctx)

    val sources = SourceMap(id, element)

    val obj = metaModel(element)

    traverseMetaModel(id, element, sources, obj, b, ctx)

    createCustomExtensions(element, b, ctx)

    val sourceMapId = if (id.endsWith("/")) {
      id + "source-map"
    } else if (id.contains("#") || id.startsWith("null")) {
      id + "/source-map"
    } else {
      id + "#/source-map"
    }
    createSourcesNode(sourceMapId, sources, b, ctx)
  }

  def traverseMetaModel(id: String,
                        element: AmfObject,
                        sources: SourceMap,
                        obj: Obj,
                        b: Entry[T],
                        ctx: EmissionContext): Unit = {
    createTypeNode(b, obj, Some(element), ctx)

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
          value(Type.Int, Value(AmfScalar(e.propertyFields().size), Annotations()), id, _ => {}, _, ctx)
        )
      case _ => // Nothing to do
    }

    modelFields.foreach { f =>
      emitStaticField(f, element, id, sources, b, ctx)
    }

  }

  private def emitStaticField(field: Field,
                              element: AmfObject,
                              id: String,
                              sources: SourceMap,
                              b: Entry[T],
                              ctx: EmissionContext): Unit = {
    element.fields.entryJsonld(field) match {
      case Some(FieldEntry(f, v)) =>
        val url = ctx.emitIri(f.value.iri())
        b.entry(
          url,
          value(f.`type`, v, id, sources.property(url), _, ctx)
        )
      case None => // Missing field
    }
  }

  private def createCustomExtensions(element: AmfObject, b: Entry[T], ctx: EmissionContext): Unit = {
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
                  createCustomExtension(b, uri, extension, None, ctx)
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
            createCustomExtension(b, uri, extension, Some(f), ctx)
            count += 1
          })
    }

    if (customProperties.nonEmpty)
      b.entry(
        ctx.emitIri(DomainElementModel.CustomDomainProperties.value.iri()),
        _.list { b =>
          customProperties.foreach(iri(b, _, ctx, inArray = true))
        }
      )
  }

  private def createCustomExtension(b: Entry[T],
                                    uri: String,
                                    extension: DomainExtension,
                                    field: Option[Field] = None,
                                    ctx: EmissionContext): Unit = {
    b.entry(
      uri,
      _.obj { b =>
        b.entry(
          ctx.emitIri(DomainExtensionModel.Name.value.iri()),
          listWithScalar(_, extension.name.value())
        )
        field.foreach(
          f =>
            b.entry(
              ctx.emitIri(DomainExtensionModel.Element.value.iri()),
              listWithScalar(_, f.value.iri())
          ))
        traverse(extension.extension, b, ctx)
      }
    )
  }

  def createSortedArray(b: Part[T],
                        seq: Seq[AmfElement],
                        parent: String,
                        element: Type,
                        sources: Value => Unit,
                        v: Option[Value] = None,
                        ctx: EmissionContext): Unit = {
    b.list {
      _.obj { b =>
        val id = s"$parent/list"
        createIdNode(b, id, ctx)
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
                            link(b, elementInArray, inArray = true, ctx)
                          case elementInArray: AmfObject =>
                            obj(b, elementInArray, inArray = true, ctx)
                        }
                      case Str =>
                        scalar(b, e, SType.Str)

                      case EncodedIri =>
                        safeIri(b, e.asInstanceOf[AmfScalar].toString, ctx, inArray = true)

                      case Iri =>
                        iri(b, e.asInstanceOf[AmfScalar].toString, ctx, inArray = true)

                      case Any =>
                        val scalarElement = e.asInstanceOf[AmfScalar]
                        scalarElement.value match {
                          case bool: Boolean =>
                            typedScalar(b, bool.toString, DataType.Boolean, ctx, inArray = true)
                          case str: String =>
                            typedScalar(b, str.toString, DataType.String, ctx, inArray = true)
                          case i: Int =>
                            typedScalar(b, i.toString, DataType.Integer, ctx, inArray = true)
                          case f: Float =>
                            typedScalar(b, f.toString, DataType.Float, ctx, inArray = true)
                          case d: Double =>
                            typedScalar(b, d.toString, DataType.Double, ctx, inArray = true)
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

  private def value(t: Type,
                    v: Value,
                    parent: String,
                    sources: Value => Unit,
                    b: Part[T],
                    ctx: EmissionContext): Unit = {
    t match {
      case _: ShapeModel
          if v.value.annotations.contains(classOf[ResolvedInheritance]) && ((!ctx.declares) || (ctx.declares && ctx
            .isDeclared(v.value)) && ctx.isDeclared(parent)) =>
        extractToLink(v.value.asInstanceOf[Shape], b, ctx)
      case t: DomainElement with Linkable if t.isLink =>
        link(b, t, inArray = false, ctx)
        sources(v)
      case _: Obj =>
        obj(b, v.value.asInstanceOf[AmfObject], inArray = false, ctx)
        sources(v)
      case Iri =>
        iri(b, v.value.asInstanceOf[AmfScalar].toString, ctx)
        sources(v)
      case EncodedIri =>
        safeIri(b, v.value.asInstanceOf[AmfScalar].toString, ctx)
        sources(v)
      case LiteralUri =>
        typedScalar(b, v.value.asInstanceOf[AmfScalar].toString, DataType.AnyUri, ctx)
        sources(v)
      case Str =>
        listWithScalar(b, v.value)
        sources(v)
      case Bool =>
        listWithScalar(b, v.value, SType.Bool)
        sources(v)
      case Type.Int =>
        listWithScalar(b, v.value, SType.Int)
        sources(v)
      case Type.Double =>
        // this will transform the value to double and will not emit @type TODO: ADD YType.Double
        listWithScalar(b, v.value, SType.Float)
        sources(v)
      case Type.Float =>
        listWithScalar(b, v.value, SType.Float)
        sources(v)
      case Type.DateTime =>
        val dateTime = v.value.asInstanceOf[AmfScalar].value.asInstanceOf[SimpleDateTime]
        typedScalar(b, emitDateFormat(dateTime), DataType.DateTime, ctx)
        sources(v)
      case Type.Date =>
        val maybeDateTime = v.value.asInstanceOf[AmfScalar].value match {
          case dt: SimpleDateTime => Some(dt)
          case other              => SimpleDateTime.parse(other.toString).toOption
        }
        maybeDateTime match {
          case Some(dateTime) =>
            if (dateTime.timeOfDay.isDefined || dateTime.zoneOffset.isDefined) {
              typedScalar(b, emitDateFormat(dateTime), DataType.DateTime, ctx)
            } else {
              typedScalar(b, f"${dateTime.year}%04d-${dateTime.month}%02d-${dateTime.day}%02d", DataType.Date, ctx)

            }
          case _ =>
            listWithScalar(b, v.value)
        }
        sources(v)
      case a: SortedArray =>
        createSortedArray(b, v.value.asInstanceOf[AmfArray].values, parent, a.element, sources, Some(v), ctx)
      case a: Array =>
        b.list { b =>
          val seq = v.value.asInstanceOf[AmfArray]
          sources(v)
          a.element match {
            case _: Obj =>
              seq.values.asInstanceOf[Seq[AmfObject]].foreach {
                case v @ (_: Shape)
                    if v.annotations
                      .contains(classOf[ResolvedInheritance]) && ((!ctx.declares) || (ctx.declares && ctx
                      .isDeclared(v) && ctx.isDeclared(parent))) =>
                  extractToLink(v.asInstanceOf[Shape], b, ctx)
                case elementInArray: DomainElement with Linkable if elementInArray.isLink =>
                  link(b, elementInArray, inArray = true, ctx)
                case elementInArray =>
                  obj(b, elementInArray, inArray = true, ctx)
              }
            case Str =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach { e =>
                scalar(b, e.toString)
              }
            case EncodedIri =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach(e => safeIri(b, e.toString, ctx, inArray = true))
            case Iri =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach(e => iri(b, e.toString, ctx, inArray = true))
            case LiteralUri =>
              typedScalar(b, v.value.asInstanceOf[AmfScalar].toString, DataType.AnyUri, ctx, inArray = true)
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
                  typedScalar(b, emitDateFormat(dateTime), DataType.DateTime, ctx)
                }
            case Type.Date =>
              seq.values
                .asInstanceOf[Seq[AmfScalar]]
                .foreach { e =>
                  val dateTime = e.value.asInstanceOf[SimpleDateTime]
                  emitSimpleDateTime(b, dateTime, inArray = false, ctx)
                }
            case Any =>
              seq.values.asInstanceOf[Seq[AmfScalar]].foreach { scalarElement =>
                scalarElement.value match {
                  case bool: Boolean =>
                    typedScalar(b, bool.toString, DataType.Boolean, ctx, inArray = true)
                  case i: Int              => typedScalar(b, i.toString, DataType.Integer, ctx, inArray = true)
                  case f: Float            => typedScalar(b, f.toString, DataType.Float, ctx, inArray = true)
                  case d: Double           => typedScalar(b, d.toString, DataType.Double, ctx, inArray = true)
                  case sdt: SimpleDateTime => emitSimpleDateTime(b, sdt, inArray = true, ctx)
                  case other               => scalar(b, other.toString)
                }
              }
            case _ => seq.values.asInstanceOf[Seq[AmfScalar]].foreach(e => iri(b, e.toString, ctx, inArray = true))
          }
        }

      case Any if v.value.isInstanceOf[AmfScalar] =>
        v.value.asInstanceOf[AmfScalar].value match {
          case bool: Boolean       => typedScalar(b, bool.toString, DataType.Boolean, ctx, inArray = true)
          case i: Int              => typedScalar(b, i.toString, DataType.Integer, ctx, inArray = true)
          case f: Float            => typedScalar(b, f.toString, DataType.Float, ctx, inArray = true)
          case d: Double           => typedScalar(b, d.toString, DataType.Double, ctx, inArray = true)
          case sdt: SimpleDateTime => emitSimpleDateTime(b, sdt, inArray = true, ctx)
          case other               => scalar(b, other.toString)
        }
    }
  }

  private def emitSimpleDateTime(b: Part[T],
                                 dateTime: SimpleDateTime,
                                 inArray: Boolean = true,
                                 ctx: EmissionContext): Unit = {
    if (dateTime.timeOfDay.isDefined || dateTime.zoneOffset.isDefined) {
      typedScalar(b, emitDateFormat(dateTime), DataType.DateTime, ctx, inArray)
    } else {
      typedScalar(b, f"${dateTime.year}%04d-${dateTime.month}%02d-${dateTime.day}%02d", DataType.Date, ctx)
    }
  }

  private def obj(b: Part[T], element: AmfObject, inArray: Boolean = false, ctx: EmissionContext): Unit = {
    def emit(b: Part[T]): Unit = {
      cache.get(element.id) match {
        case Some(value) => b.+=(value)
        case None        => b.obj(traverse(element, _, ctx)).foreach(cache.put(element.id, _))
      }
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def extractToLink(shape: Shape, b: Part[T], ctx: EmissionContext): Unit = {
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

    link(b, linked, inArray = false, ctx)
  }

  private def link(b: Part[T],
                   elementWithLink: DomainElement with Linkable,
                   inArray: Boolean = false,
                   ctx: EmissionContext): Unit = {
    def emit(b: Part[T]): Unit = {
      // before emitting, we remove the link target to avoid loops and set
      // the fresh value for link-id
      val savedLinkTarget = elementWithLink.linkTarget
      elementWithLink.linkTarget.foreach { target =>
        elementWithLink.set(LinkableElementModel.TargetId, target.id)
        elementWithLink.fields.removeField(LinkableElementModel.Target)
      }
      b.obj { o =>
        traverse(elementWithLink, o, ctx)
      }
      // we reset the link target after emitting
      savedLinkTarget.foreach { target =>
        elementWithLink.fields.setWithoutId(LinkableElementModel.Target, target)
      }
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def iri(b: Part[T], content: String, ctx: EmissionContext, inArray: Boolean = false): Unit = {
    // Last update, we assume that the iris are valid and han been encoded. Other option could be use the previous custom lcoal object URLEncoder but not search for %.
    // That can be a problem, because some chars could not being encoded
    def emit(b: Part[T]): Unit = {
      b.obj(_.entry("@id", raw(_, ctx.emitId(content))))
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def safeIri(b: Part[T], content: String, ctx: EmissionContext, inArray: Boolean = false): Unit = {
    def emit(b: Part[T]): Unit = {
      b.obj(_.entry("@id", raw(_, ctx.emitId(content))))
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def typedScalar(b: Part[T],
                          content: String,
                          dataType: String,
                          ctx: EmissionContext,
                          inArray: Boolean = false): Unit = {
    def emit(b: Part[T]): Unit = b.obj { m =>
      m.entry("@value", raw(_, content))
      m.entry("@type", raw(_, ctx.emitIri(dataType)))
    }

    if (inArray) emit(b) else b.list(emit)
  }

  private def createIdNode(b: Entry[T], id: String, ctx: EmissionContext): Unit = b.entry(
    "@id",
    raw(_, ctx.emitId(id))
  )

  private def createTypeNode(b: Entry[T],
                             obj: Obj,
                             maybeElement: Option[AmfObject] = None,
                             ctx: EmissionContext): Unit = {
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

  private def createSourcesNode(id: String, sources: SourceMap, b: Entry[T], ctx: EmissionContext): Unit = {
    if (options.isWithSourceMaps && sources.nonEmpty) {
      if (options.isWithRawSourceMaps) {
        b.entry(
          "smaps",
          _.obj { b =>
            createAnnotationNodes(b, sources.annotations, ctx)
            createAnnotationNodes(b, sources.eternals, ctx)
          }
        )
      } else {
        b.entry(
          ctx.emitIri(DomainElementModel.Sources.value.iri()),
          _.list {
            _.obj { b =>
              createIdNode(b, id, ctx)
              createTypeNode(b, SourceMapModel, None, ctx)
              createAnnotationNodes(b, sources.annotations, ctx)
              createAnnotationNodes(b, sources.eternals, ctx)
            }
          }
        )
      }
    } else {
      createEternalsAnnotationsNodes(id, options, b, sources, ctx)
    }
  }

  private def createEternalsAnnotationsNodes(id: String,
                                             options: RenderOptions,
                                             b: Entry[T],
                                             sources: SourceMap,
                                             ctx: EmissionContext): Unit = {
    if (sources.eternals.nonEmpty)
      if (options.isWithRawSourceMaps) {
        b.entry(
          "smaps",
          _.obj { b =>
            createAnnotationNodes(b, sources.eternals, ctx)
          }
        )
      } else {
        b.entry(
          ctx.emitIri(DomainElementModel.Sources.value.iri()),
          _.list {
            _.obj { b =>
              createIdNode(b, id, ctx)
              createTypeNode(b, SourceMapModel, None, ctx)
              createAnnotationNodes(b, sources.eternals, ctx)
            }
          }
        )
      }
  }

  private def createAnnotationNodes(b: Entry[T],
                                    annotations: mutable.ListMap[String, mutable.ListMap[String, String]],
                                    ctx: EmissionContext): Unit = {
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
            _.list(b => values.foreach(createAnnotationValueNode(b, _, ctx)))
          )
        }
    })
  }

  private def createAnnotationValueNode(b: Part[T], tuple: (String, String), ctx: EmissionContext): Unit =
    tuple match {
      case (iri, v) =>
        b.obj { b =>
          b.entry(ctx.emitIri(SourceMapModel.Element.value.iri()), listWithScalar(_, iri))
          b.entry(ctx.emitIri(SourceMapModel.Value.value.iri()), listWithScalar(_, v))
        }
    }

  private def emitDateFormat(dateTime: SimpleDateTime) = dateTime.rfc3339

  class EmissionContext(val prefixes: mutable.Map[String, String],
                        var base: String,
                        val options: RenderOptions,
                        var declares: Boolean = false) {
    var counter: Int = 1

    private val declarations: mutable.LinkedHashSet[AmfElement] = mutable.LinkedHashSet.empty

    private val typeCount: IdCounter = new IdCounter()

    def nextTypeName: String = typeCount.genId("amf_inline_type")

    def declares(d: Boolean): this.type = {
      declares = d
      this
    }

    def +(element: AmfElement): this.type = {
      declarations += element
      this
    }

    def ++(elements: Iterable[AmfElement]): this.type = {
      declarations ++= elements
      this
    }

    def isDeclared(e: AmfElement): Boolean = declarations.contains(e)

    def isDeclared(id: String): Boolean =
      declarations.collect({ case obj: AmfObject if obj.id.equals(id) => obj }).nonEmpty

    def declared: Seq[AmfElement] = declarations.toSeq

    def shouldCompact: Boolean                           = options.isCompactUris
    protected def compactAndCollect(uri: String): String = Namespace.compactAndCollect(uri, prefixes)

    def emitIri(uri: String): String = if (shouldCompact) compactAndCollect(uri) else uri

    def emitId(uri: String): String = if (shouldCompact && uri.contains(base)) uri.replace(base, "") else uri
    def setupContextBase(location: String): Unit = {
      if (Option(location).isDefined) {
        base = if (location.replace("://", "").contains("/")) {
          val basePre = if (location.contains("#")) {
            location.split("#").head
          } else {
            location
          }
          val parts = basePre.split("/").dropRight(1)
          parts.mkString("/")
        } else {
          location.split("#").head
        }
      } else {
        base = ""
      }
    }

    def emitContext(b: Entry[T]): Unit = {
      if (shouldCompact)
        b.entry("@context", _.obj { b =>
          b.entry("@base", base)
          prefixes.foreach {
            case (p, v) =>
              b.entry(p, v)
          }
        })
    }
  }

  object EmissionContext {
    def apply(unit: BaseUnit, options: RenderOptions) =
      new EmissionContext(mutable.Map(), unit.id, options)
  }

  private def scalar(b: Part[T], content: String, t: SType): Unit = b.obj(_.entry("@value", Scalar(t, content)))

  private def scalar(b: Part[T], content: String): Unit = scalar(b, content, SType.Str)
  private def scalar(b: Part[T], content: AmfElement, t: SType): Unit =
    scalar(b, content.asInstanceOf[AmfScalar].value.toString, t)

  private def listWithScalar(b: Part[T], content: String, t: SType): Unit = b.list(scalar(_, content, t))
  private def listWithScalar(b: Part[T], content: String): Unit           = listWithScalar(b, content, SType.Str)

  private def listWithScalar(b: Part[T], content: AmfElement, t: SType): Unit = b.list(scalar(_, content, t))
  private def listWithScalar(b: Part[T], content: AmfElement): Unit           = listWithScalar(b, content, SType.Str)

}
