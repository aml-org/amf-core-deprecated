package amf.plugins.document.graph.parser

import amf.core.annotations._
import amf.core.benchmark.ExecutionLog
import amf.core.emitter.RenderOptions
import amf.core.metamodel.Type.{Any, Array, Bool, EncodedIri, Iri, LiteralUri, SortedArray, Str}
import amf.core.metamodel._
import amf.core.metamodel.document.{ModuleModel, SourceMapModel}
import amf.core.metamodel.domain.extensions.DomainExtensionModel
import amf.core.metamodel.domain.{DomainElementModel, LinkableElementModel, ShapeModel}
import amf.core.model.document.{BaseUnit, SourceMap}
import amf.core.model.domain.DataNodeOps.adoptTree
import amf.core.model.domain._
import amf.core.model.domain.extensions.DomainExtension
import amf.core.parser.{Annotations, FieldEntry, Value}
import amf.core.utils._
import amf.core.vocabulary.{Namespace, ValueType}
import org.mulesoft.common.io.Output
import org.mulesoft.common.io.Output._
import org.mulesoft.common.time.SimpleDateTime
import org.yaml.builder.DocBuilder
import org.yaml.builder.DocBuilder.SType

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.scalajs.js

object ScalaJSJsonLdEmitter {

  def emit[W: Output](unit: BaseUnit, writer: W, renderOptions: RenderOptions = RenderOptions()): Boolean = {
    val generated =ExecutionLog.withStage("ScalaJSJSONLdEmitter#emit: Generating JSON-LD") {
      new ScalaJSJsonLdEmitter(renderOptions).root(unit)
    }
    ExecutionLog.withStage("ScalaJSJSONLdEmitter#emit: Writing JSON-LD") {
      writer.append(js.JSON.stringify(generated))
    }
    true
  }
}

class ScalaJSJsonLdEmitter(val options: RenderOptions) extends MetaModelTypeMapping {

  def updateObj(o: js.Dictionary[js.Any], key: String, v: js.Any): js.Dictionary[js.Any] = {
    o.update(key, v)
    o
  }

  def root(unit: BaseUnit): js.Dictionary[js.Any] = {
    val ctx                            = EmissionContext(unit, options)
    val entry: Option[FieldEntry]      = unit.fields.entry(ModuleModel.Declares)
    val elements: Iterable[AmfElement] = entry.map(_.value.value.asInstanceOf[AmfArray].values).getOrElse(Nil)
    ctx ++ elements
    unit.fields.removeField(ModuleModel.Declares)

    var top = js.Dictionary[js.Any]()
    top = traverse(unit, top, ctx)
    top = emitDeclarations(top, unit.id, SourceMap(unit.id, unit), ctx)
    entry.foreach(e => unit.fields.setWithoutId(ModuleModel.Declares, e.array))
    ctx.emitContext(top)
  }

  private def emitDeclarations(b: js.Dictionary[js.Any], id: String, sources: SourceMap, ctx: EmissionContext): js.Dictionary[js.Any] = {
    if (ctx.declared.nonEmpty) {
      val v   = Value(AmfArray(ctx.declared), Annotations())
      val f   = ModuleModel.Declares
      val url = ctx.emitIri(f.value.iri())
      updateObj(b, url, value(f.`type`, v, id, sources.property(url), ctx.declares(true)))
    } else {
      ctx.declares(false)
      b
    }
  }

  // Cache for already emitted JSON objects
  var jsonCache = Map[String, js.Dictionary[js.Any]]()

  def traverse(element: AmfObject, b: js.Dictionary[js.Any], ctx: EmissionContext): js.Dictionary[js.Any] = {
    val id = element.id
    if (jsonCache.get(element.id).isDefined) { jsonCache(id)}
    else {
      var traversed = b

      traversed = createIdNode(traversed, id, ctx)

      val sources = SourceMap(id, element)

      val obj = metaModel(element)

      traversed = traverseMetaModel(id, element, sources, obj, traversed, ctx)

      var res = createCustomExtensions(element, traversed, ctx)
      jsonCache += (id -> res)


      val sourceMapId = if (id.endsWith("/")) {
        id + "source-map"
      } else if (id.contains("#") || id.startsWith("null")) {
        id + "/source-map"
      } else {
        id + "#/source-map"
      }

      createSourcesNode(sourceMapId, sources, b, ctx)
      res
    }
  }

  def traverseMetaModel(id: String,
                        element: AmfObject,
                        sources: SourceMap,
                        obj: Obj,
                        b: js.Dictionary[js.Any],
                        ctx: EmissionContext): js.Dictionary[js.Any] = {
    var traversed = createTypeNode(b, obj, Some(element), ctx)

    // workaround for lazy values in shape
    val modelFields = obj.fields ++ (obj match {
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
        traversed = updateObj(traversed, url, value(Type.Int, Value(AmfScalar(e.properties.size), Annotations()), id, _ => {}, ctx))
      case _ => // Nothing to do
    }

    obj match {
      case dynamic: DynamicObj =>
        modelFields.foreach { f =>
          traversed = emitDynamicField(f, traversed, id, element.asInstanceOf[DynamicDomainElement], sources, ctx)
        }
      case _ =>
        modelFields.foreach { f =>
          traversed = emitStaticField(f, element, id, sources, traversed, ctx)
        }
    }

    traversed
  }

  private def emitStaticField(field: Field,
                              element: AmfObject,
                              id: String,
                              sources: SourceMap,
                              b: js.Dictionary[js.Any],
                              ctx: EmissionContext): js.Dictionary[js.Any] = {
    element.fields.entryJsonld(field) match {
      case Some(FieldEntry(f, v)) =>
        val url = ctx.emitIri(f.value.iri())
        updateObj(
          b,
          url,
          value(f.`type`, v, id, sources.property(url), ctx)
        )
      case _ =>
        b // Missing field
    }
  }

  private def emitDynamicField(f: Field,
                               b: js.Dictionary[js.Any],
                               id: String,
                               element: DynamicDomainElement,
                               sources: SourceMap,
                               ctx: EmissionContext): js.Dictionary[js.Any] = {
    element.valueForField(f) match {
      case Some(amfValue) =>
        val url = ctx.emitIri(f.value.iri())
        element match {
          case schema: DynamicDomainElement if !schema.isInstanceOf[ExternalSourceElement] =>
            updateObj(
              b,
              url,
              value(f.`type`, Value(amfValue.value, amfValue.value.annotations), id, sources.property(url), ctx)
            )
          case _ =>
            updateObj(
              b,
              url,
              value(f.`type`, amfValue, id, sources.property(url), ctx)
            )
        }
      case _            =>
        b
    }
  }

  private def createCustomExtensions(element: AmfObject, b: js.Dictionary[js.Any], ctx: EmissionContext): js.Dictionary[js.Any] = {
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

    if (customProperties.nonEmpty) {
      val cps = customProperties.map(iri(_, ctx, inArray = true))
      val cpsArray = js.Array(cps:_*)
      updateObj(b,
        ctx.emitIri(DomainElementModel.CustomDomainProperties.value.iri()),
        cpsArray
      )
    } else {
      b
    }
  }

  private def createCustomExtension(b: js.Dictionary[js.Any],
                                    uri: String,
                                    extension: DomainExtension,
                                    field: Option[Field] = None,
                                    ctx: EmissionContext): js.Dictionary[js.Any] = {



    val fieldObj = field match {
      case Some(f) =>
        js.Dictionary[js.Any](
          ctx.emitIri(DomainExtensionModel.Name.value.iri())    ->listWithScalar(extension.name.value()),
          ctx.emitIri(DomainExtensionModel.Element.value.iri()) -> listWithScalar(f.value.iri())
        )
      case _        =>
        js.Dictionary[js.Any](ctx.emitIri(DomainExtensionModel.Name.value.iri()) -> listWithScalar(extension.name.value()))
    }
    val extensionObj = traverse(extension.extension, fieldObj, ctx)
    updateObj(b, uri, extensionObj)
  }

  def createSortedArray(seq: Seq[AmfElement],
                        parent: String,
                        element: Type,
                        sources: Value => Unit,
                        v: Option[Value] = None,
                        ctx: EmissionContext): js.Array[js.Any] = {
    val members = seq.zipWithIndex.foldLeft(new mutable.LinkedHashMap[String, js.Any]()) {
      case (acc, (e, i)) =>

        val memberId = ctx.emitIri((Namespace.Rdfs + s"_${i + 1}").iri())

        val value: js.Any = element match {
          case _: Obj =>
            e match {
              case elementInArray: DomainElement with Linkable if elementInArray.isLink =>
                link(elementInArray, inArray = true, ctx)
              case elementInArray: AmfObject =>
                obj(elementInArray, inArray = true, ctx)
            }
          case Str =>
            scalar(e, SType.Str)

          case EncodedIri =>
            iri(e.asInstanceOf[AmfScalar].toString, ctx, inArray = true)

          case Iri =>
            iri(e.asInstanceOf[AmfScalar].toString, ctx, inArray = true)

          case Any =>
            val scalarElement = e.asInstanceOf[AmfScalar]
            scalarElement.value match {
              case bool: Boolean =>
                typedScalar(bool.toString, (Namespace.Xsd + "boolean").iri(), ctx, inArray = true)
              case str: String =>
                typedScalar(str.toString, (Namespace.Xsd + "string").iri(), ctx, inArray = true)
              case i: Int =>
                typedScalar(i.toString, (Namespace.Xsd + "integer").iri(), ctx, inArray = true)
              case f: Float =>
                typedScalar(f.toString, (Namespace.Xsd + "float").iri(), ctx, inArray = true)
              case d: Double =>
                typedScalar(d.toString, (Namespace.Xsd + "double").iri(), ctx, inArray = true)
              case other => scalar(other.toString)
            }
        }
        acc.put(memberId, value)
        acc
    }

    val id = s"$parent/list"
    members.put("@type", ctx.emitIri((Namespace.Rdfs + "Seq").iri()))
    val seqJson = js.Dictionary[js.Any](members.toSeq:_*)
    createIdNode(seqJson, id, ctx)
    js.Array[js.Any](seqJson)
  }

  private def value(t: Type, v: Value, parent: String, sources: Value => Unit, ctx: EmissionContext): js.Any = {
    t match {
      case _: ShapeModel
        if v.value.annotations.contains(classOf[ResolvedInheritance]) && ((!ctx.declares) || (ctx.declares && ctx.isDeclared(v.value)) && ctx.isDeclared(parent)) =>
        extractToLink(v.value.asInstanceOf[Shape], ctx)
      case t: DomainElement with Linkable if t.isLink =>
        link(t, inArray = false, ctx)
      // sources(v)
      case _: Obj =>
        obj(v.value.asInstanceOf[AmfObject], inArray = false, ctx)
      // sources(v)
      case Iri =>
        iri(v.value.asInstanceOf[AmfScalar].toString, ctx)
      //sources(v)
      case EncodedIri =>
        iri(v.value.asInstanceOf[AmfScalar].toString, ctx)
      //sources(v)
      case LiteralUri =>
        typedScalar(v.value.asInstanceOf[AmfScalar].toString, (Namespace.Xsd + "anyURI").iri(), ctx)
      //sources(v)
      case Str =>
        v.annotations.find(classOf[ScalarType]) match {
          case Some(annotation) =>
            typedScalar(v.value.asInstanceOf[AmfScalar].toString, annotation.datatype, ctx)
          case None =>
            listWithScalar(v.value)
        }
      //sources(v)
      case Bool =>
        listWithScalar(v.value, SType.Bool)
      //sources(v)
      case Type.Int =>
        listWithScalar(v.value, SType.Int)
      //sources(v)
      case Type.Double =>
        // this will transform the value to double and will not emit @type TODO: ADD YType.Double
        listWithScalar(v.value, SType.Float)
      //sources(v)
      case Type.Float =>
        listWithScalar(v.value, SType.Float)
      //sources(v)
      case Type.DateTime =>
        val dateTime = v.value.asInstanceOf[AmfScalar].value.asInstanceOf[SimpleDateTime]
        typedScalar(emitDateFormat(dateTime), (Namespace.Xsd + "dateTime").iri(), ctx)
      //sources(v)
      case Type.Date =>
        val maybeDateTime = v.value.asInstanceOf[AmfScalar].value match {
          case dt: SimpleDateTime => Some(dt)
          case other              => SimpleDateTime.parse(other.toString).toOption
        }
        maybeDateTime match {
          case Some(dateTime) =>
            if (dateTime.timeOfDay.isDefined || dateTime.zoneOffset.isDefined) {
              typedScalar(emitDateFormat(dateTime), (Namespace.Xsd + "dateTime").iri(), ctx)
            } else {
              typedScalar(f"${dateTime.year}%04d-${dateTime.month}%02d-${dateTime.day}%02d",
                (Namespace.Xsd + "date").iri(),
                ctx)

            }
          case _ =>
            listWithScalar(v.value)
        }
      //sources(v)
      case a: SortedArray =>
        createSortedArray(v.value.asInstanceOf[AmfArray].values, parent, a.element, sources, Some(v), ctx)
      case a: Array =>
        val seq = v.value.asInstanceOf[AmfArray]
        val jsonValues: Seq[js.Any] = a.element match {
          case _: Obj =>
            seq.values.asInstanceOf[Seq[AmfObject]].map {
              case v @ (_: Shape)
                if v.annotations
                  .contains(classOf[ResolvedInheritance]) && ((!ctx.declares) || (ctx.declares && ctx
                  .isDeclared(v) && ctx.isDeclared(parent))) =>
                extractToLink(v.asInstanceOf[Shape], ctx)
              case elementInArray: DomainElement with Linkable if elementInArray.isLink =>
                link(elementInArray, inArray = true, ctx)
              case elementInArray =>
                obj(elementInArray, inArray = true, ctx)
            }
          case Str =>
            seq.values.asInstanceOf[Seq[AmfScalar]].map { e =>
              e.annotations.find(classOf[ScalarType]) match {
                case Some(annotation) =>
                  typedScalar(e.value.asInstanceOf[AmfScalar].toString, annotation.datatype, ctx, inArray = true)
                case None => scalar(e.toString)
              }
            }
          case EncodedIri =>
            seq.values.asInstanceOf[Seq[AmfScalar]].map(e => iri(e.toString, ctx, inArray = true))
          case Iri =>
            seq.values.asInstanceOf[Seq[AmfScalar]].map(e => iri(e.toString, ctx, inArray = true))
          case LiteralUri =>
            seq.values
              .asInstanceOf[Seq[AmfScalar]]
              .map(e => typedScalar(
                v.value.asInstanceOf[AmfScalar].toString,
                (Namespace.Xsd + "anyURI").iri(),
                ctx,
                inArray = true))
          case Type.Int =>
            seq.values
              .asInstanceOf[Seq[AmfScalar]]
              .map(e => scalar(e.value.toString, SType.Int))
          case Type.Float =>
            seq.values
              .asInstanceOf[Seq[AmfScalar]]
              .map(e => scalar(e.value.toString, SType.Float))
          case Bool =>
            seq.values
              .asInstanceOf[Seq[AmfScalar]]
              .map(e => scalar(e.value.toString, SType.Bool))
          case Type.DateTime =>
            seq.values
              .asInstanceOf[Seq[AmfScalar]]
              .map { e =>
                val dateTime = e.value.asInstanceOf[SimpleDateTime]
                typedScalar(emitDateFormat(dateTime), (Namespace.Xsd + "dateTime").iri(), ctx)
              }
          case Type.Date =>
            seq.values
              .asInstanceOf[Seq[AmfScalar]]
              .map { e =>
                val dateTime = e.value.asInstanceOf[SimpleDateTime]
                emitSimpleDateTime(dateTime, inArray = false, ctx)
              }
          case Any =>
            seq.values.asInstanceOf[Seq[AmfScalar]].map { scalarElement =>
              scalarElement.value match {
                case bool: Boolean =>
                  typedScalar(bool.toString, (Namespace.Xsd + "boolean").iri(), ctx, inArray = true)
                case i: Int              => typedScalar(i.toString, (Namespace.Xsd + "integer").iri(), ctx, inArray = true)
                case f: Float            => typedScalar(f.toString, (Namespace.Xsd + "float").iri(), ctx, inArray = true)
                case d: Double           => typedScalar(d.toString, (Namespace.Xsd + "double").iri(), ctx, inArray = true)
                case sdt: SimpleDateTime => emitSimpleDateTime(sdt, inArray = true, ctx)
                case other               => scalar(other.toString)
              }
            }
          case _ => seq.values.asInstanceOf[Seq[AmfScalar]].map(e => iri(e.toString, ctx, inArray = true))
        }

        js.Array[js.Any](jsonValues:_*)
    }
  }

  private def emitSimpleDateTime(dateTime: SimpleDateTime,
                                 inArray: Boolean = true,
                                 ctx: EmissionContext): js.Any = {
    if (dateTime.timeOfDay.isDefined || dateTime.zoneOffset.isDefined) {
      typedScalar(emitDateFormat(dateTime), (Namespace.Xsd + "dateTime").iri(), ctx, inArray)
    } else {
      typedScalar(
        f"${dateTime.year}%04d-${dateTime.month}%02d-${dateTime.day}%02d",
        (Namespace.Xsd + "date").iri(),
        ctx)
    }
  }

  private def obj(element: AmfObject, inArray: Boolean = false, ctx: EmissionContext): js.Any = {
    val jsonObj = traverse(element, js.Dictionary[js.Any](), ctx)
    if (inArray) jsonObj else js.Array[js.Any](jsonObj)
  }

  private def extractToLink(shape: Shape, ctx: EmissionContext): js.Any = {
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

    link(linked, inArray = false, ctx)
  }

  private def link(elementWithLink: DomainElement with Linkable,
                   inArray: Boolean = false,
                   ctx: EmissionContext): js.Any = {

    // before emitting, we remove the link target to avoid loops and set
    // the fresh value for link-id
    val savedLinkTarget = elementWithLink.linkTarget
    elementWithLink.linkTarget.foreach { target =>
      elementWithLink.set(LinkableElementModel.TargetId, target.id)
      elementWithLink.fields.removeField(LinkableElementModel.Target)
    }
    val nested = traverse(elementWithLink, js.Dictionary[js.Any](), ctx)
    // we reset the link target after emitting
    savedLinkTarget.foreach { target =>
      elementWithLink.fields.setWithoutId(LinkableElementModel.Target, target)
    }

    if (inArray) {
      nested
    } else {
      js.Array[js.Any](nested)
    }
  }

  private def iri(content: String, ctx: EmissionContext, inArray: Boolean = false): js.Any = {
    // Last update, we assume that the iris are valid and han been encoded. Other option could be use the previous custom lcoal object URLEncoder but not search for %.
    // That can be a problem, because some chars could not being encoded
    val jsonObj = js.Dictionary[js.Any]("@id" -> ctx.emitId(content))
    if (inArray) jsonObj else js.Array[js.Any](jsonObj)
  }

  private def typedScalar(content: String,
                          dataType: String,
                          ctx: EmissionContext,
                          inArray: Boolean = false): js.Any = {
    val jsonObj = js.Dictionary[js.Any](
      "@value" -> content,
      "@type" -> ctx.emitIri(dataType)
    )

    if (inArray) jsonObj else js.Array[js.Any](jsonObj)
  }

  private def createIdNode(obj: js.Dictionary[js.Any], id: String, ctx: EmissionContext): js.Dictionary[js.Any] = updateObj(obj, "@id", ctx.emitId(id))

  private def createTypeNode(b: js.Dictionary[js.Any], obj: Obj, maybeElement: Option[AmfObject] = None, ctx: EmissionContext): js.Dictionary[js.Any] = {
    val allTypes = obj.`type`.map(_.iri()).distinct.map(t => ctx.emitIri(t))
    updateObj(b, "@type", js.Array[js.Any](allTypes.asInstanceOf[Seq[js.Any]]:_*))
  }


  /*
    private def raw(b: Part, content: String): Unit =
      b += content
  */
  private def createSourcesNode(id: String, sources: SourceMap, b: js.Dictionary[js.Any], ctx: EmissionContext): Unit = {
    if (options.isWithSourceMaps && sources.nonEmpty) {
      if (options.isWithRawSourceMaps) {
        val smapsObj = js.Dictionary[js.Any]()
        createAnnotationNodes(smapsObj, sources.annotations, ctx)
        createAnnotationNodes(smapsObj, sources.eternals, ctx)
        updateObj(b, "smaps", smapsObj)
      } else {
        val smapsObj = js.Dictionary[js.Any]()
        createIdNode(smapsObj, id, ctx)
        createTypeNode(smapsObj, SourceMapModel, None, ctx)
        createAnnotationNodes(smapsObj, sources.annotations, ctx)
        createAnnotationNodes(smapsObj, sources.eternals, ctx)
        updateObj(b, ctx.emitIri(DomainElementModel.Sources.value.iri()), smapsObj)
      }
    } else {
      createEternalsAnnotationsNodes(id, options, b, sources, ctx)
    }
  }

  private def createEternalsAnnotationsNodes(id: String,
                                             options: RenderOptions,
                                             b: js.Dictionary[js.Any],
                                             sources: SourceMap,
                                             ctx: EmissionContext): Unit = {
    if (sources.eternals.nonEmpty)
      if (options.isWithRawSourceMaps) {
        val smapsObj = js.Dictionary[js.Any]()
        createAnnotationNodes(smapsObj, sources.eternals, ctx)
        updateObj(b, "smaps", smapsObj)
      } else {
        val smapsObj = js.Dictionary[js.Any]()
        createIdNode(smapsObj, id, ctx)
        createTypeNode(smapsObj, SourceMapModel, None, ctx)
        createAnnotationNodes(smapsObj, sources.eternals, ctx)
        updateObj(b, ctx.emitIri(DomainElementModel.Sources.value.iri()), smapsObj)
      }
  }

  private def createAnnotationNodes(b: js.Dictionary[js.Any],
                                    annotations: mutable.ListMap[String, mutable.ListMap[String, String]],
                                    ctx: EmissionContext): Unit = {
    annotations.foreach {
      case (a: String, values) =>
        if (ctx.options.isWithRawSourceMaps) {
          val o = js.Dictionary[js.Any]()
          values.foreach {
            case (iri, v) =>
              updateObj(o, ctx.emitId(ctx.emitId(iri)), v)

          }
          updateObj(b, a, o)
        } else {
          val o = js.Dictionary[js.Any]()
          val sourceMapsVals = values.toSeq.map(t => createAnnotationValueNode(t, ctx))
          val sourceMapsArr = js.Array(sourceMapsVals:_*)
          val sm: String = ctx.emitIri(ValueType(Namespace.SourceMaps, a).iri())
          updateObj(b, sm, sourceMapsArr)
        }
    }

  }

  private def createAnnotationValueNode(tuple: (String, String), ctx: EmissionContext): js.Dictionary[js.Any] =
    tuple match {
      case (iri, v) =>
        js.Dictionary[js.Any](
          ctx.emitIri(SourceMapModel.Element.value.iri()) -> listWithScalar(iri),
          ctx.emitIri(SourceMapModel.Value.value.iri()) -> listWithScalar(v)
        )
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

    def emitContext(b: js.Dictionary[js.Any]): js.Dictionary[js.Any] = {
      if (shouldCompact) {
        var context = js.Dictionary[js.Any]("@base" -> base)
        prefixes.foreach {
          case (p, v) =>
            context = updateObj(context, p, v)
        }
        updateObj(b, "@context", context)
      } else {
        b
      }
    }
  }

  object EmissionContext {
    def apply(unit: BaseUnit, options: RenderOptions) =
      new EmissionContext(mutable.Map(), unit.id, options)
  }

  object JSONScalarConverter {

    def apply(value: String): js.Any  = value
    def apply(value: Long): js.Any    = value
    def apply(value: Double): js.Any  = value
    def apply(value: Boolean): js.Any = value

    def apply(t: SType, value: String): js.Any = try {
      t match {
        case DocBuilder.SType.Str   => value
        case DocBuilder.SType.Int   => value.toLong
        case DocBuilder.SType.Float => value.toDouble
        case DocBuilder.SType.Bool  => value.toBoolean
      }
    } catch {
      case _: Throwable => value
    }
  }

  private def scalar(content: String, t: SType): js.Any = js.Dictionary[js.Any]("@value" -> JSONScalarConverter(t, content))
  private def scalar(content: String): js.Any = js.Dictionary[js.Any]("@value" -> content)
  private def scalar(content: AmfElement, t: SType): js.Any = scalar(content.asInstanceOf[AmfScalar].value.toString, t)

  private def listWithScalar(content: String, t: SType): js.Array[js.Any] = js.Array[js.Any](scalar(content, t))
  private def listWithScalar(content: String): js.Array[js.Any]           = listWithScalar(content, SType.Str)

  private def listWithScalar(content: AmfElement, t: SType): js.Array[js.Any] = js.Array[js.Any](scalar(content, t))
  private def listWithScalar(content: AmfElement): js.Array[js.Any]           = listWithScalar(content, SType.Str)

}
