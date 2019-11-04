package info.cemu.download

import java.io.File
import java.net.URL

import info.cemu.download.util.{IO, ProgressBar}
import info.cemu.download.util.Types._
import info.cemu.download.util.IO._

object Main {

  val buffer = byteArray(1024 * 1024)

  def download(input: URL, output: File)(implicit progressBar: Option[ProgressBar] = None): Unit = {
    lazy val tmdOutput = output.outputStream()
    var readSize: Int = 0
    try {

      val stream = input.openStream()

      try {

        while ( {
          readSize = stream.read(buffer, 0, buffer.length);
          readSize > -1
        }) {
          tmdOutput.write(buffer, 0, readSize)
          progressBar.foreach {
            _.add(readSize)
          }
        }

      } finally {
        stream.close()
      }

    } finally {
      if (readSize != 0)
        tmdOutput.close()
    }
  }

  def main(args: Array[String]): Unit = {


    val titleId = "000500001010f300"

    val tmdURL = new URL(s"http://ccs.cdn.c.shop.nintendowifi.net/ccs/download/${titleId}/tmd")

    val rootDir = new File(titleId)
    rootDir.mkdirs()
    val tmdOutputFile = new File(rootDir, "title.tmd")

    download(tmdURL, tmdOutputFile)

    val titleMetaData = TitleMetaData(tmdOutputFile.readBytes())

    val chunks = titleMetaData.contentIterator().toSeq

    val max = chunks.foldLeft(0L) {
      case (size, content) =>
        size + content.size()
    }

    implicit val progressBar = Some(ProgressBar(max))

    chunks.foreach {
      case content =>

        val outputContentFile = new File(rootDir, content.filename())

        if (!outputContentFile.exists() || outputContentFile.length() != content.size()) {
          download(new URL(s"http://ccs.cdn.c.shop.nintendowifi.net/ccs/download/${titleId}/${content.filenameBase()}"),
            outputContentFile)
        } else {
          progressBar.foreach {
            _.add(content.size())
          }
        }

        try {
          download(new URL(s"http://ccs.cdn.c.shop.nintendowifi.net/ccs/download/${titleId}/${content.filenameBase()}.h3"),
            new File(rootDir, content.filenameBase() + ".h3"))
        } catch {
          case _: Exception =>
        }
    }

  }

}
