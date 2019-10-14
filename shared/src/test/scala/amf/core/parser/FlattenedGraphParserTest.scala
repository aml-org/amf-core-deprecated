package amf.core.parser

import amf.Core
import amf.client.convert.{BaseUnitConverter, NativeOps}
import amf.client.model.document.{BaseUnit, Document}
import amf.client.model.domain.{ScalarNode => Scalar}
import amf.client.parse.AmfGraphParser
import amf.core.io.FileAssertionTest
import amf.core.render.ElementsFixture
import org.scalatest.{AsyncFunSuite, Matchers}

import scala.concurrent.{ExecutionContext, Future}

trait FlattenedGraphParserTest
    extends AsyncFunSuite
    with NativeOps
    with FileAssertionTest
    with BaseUnitConverter
    with Matchers
    with ElementsFixture {

  override implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

  test("Test parse simple document") {
    Core.init().asFuture.flatMap { _ =>
      val golden              = "shared/src/test/resources/parser/simple-document.flattened.jsonld"
      val f: Future[BaseUnit] = new AmfGraphParser().parseFileAsync("file://" + golden).asFuture

      f.map { u =>
        u.location shouldBe "file://" + golden
        u.isInstanceOf[Document] shouldBe true
        val doc = u.asInstanceOf[Document]
        doc.encodes.isInstanceOf[Scalar] shouldBe true
        val declared = doc.declares.asSeq.head
        declared.isInstanceOf[amf.client.model.domain.ArrayNode] shouldBe true
      }
    }
  }

}
