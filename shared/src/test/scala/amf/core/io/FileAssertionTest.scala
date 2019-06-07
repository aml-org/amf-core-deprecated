package amf.core.io

import amf.core.unsafe.PlatformSecrets
import amf.core.common.Tests.checkDiff
import org.mulesoft.common.io.{AsyncFile, FileSystem}
import org.scalatest.Assertion

import scala.concurrent.{ExecutionContext, Future}

trait FileAssertionTest extends PlatformSecrets {

  private implicit val executionContext: ExecutionContext =
    ExecutionContext.Implicits.global

  protected val fs: FileSystem = platform.fs

  protected def writeTemporaryFile(golden: String)(
      content: String): Future[AsyncFile] = {
    val file = tmp(s"${golden.replaceAll("/", "-")}.tmp")
    val actual = fs.asyncFile(file)
    actual.write(content).map(_ => actual)
  }

  protected def assertDifferences(actual: AsyncFile,
                                  golden: String): Future[Assertion] = {
    val expected = fs.asyncFile(golden)
    expected.read().flatMap(_ => checkDiff(actual, expected))
  }

  private def withTrailingSeparator(dir: String, char: Char): String =
    if (dir.endsWith(char.toString)) dir
    else dir.concat(char.toString)

  /** Return random temporary file name for testing. */
  def tmp(name: String = ""): String =
    withTrailingSeparator(platform.tmpdir(), platform.fs.separatorChar) + System
      .nanoTime() + "-" + name
}
