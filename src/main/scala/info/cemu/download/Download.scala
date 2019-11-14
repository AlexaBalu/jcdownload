package info.cemu.download

import java.io.{File}
import java.net.URL

import info.cemu.download.util.{ProgressBar}
import info.cemu.download.util.Types._
import info.cemu.download.util.IO._

object Download {

  def main(args: Array[String]): Unit = {

    if (args.length < 2)
      throw new RuntimeException("java -jar jcdownload.jar <common key> <title key>")

    val db = Database(args(0).hb)

    val titleKey = args(1).hb

    db.findTitle(titleKey) match {
      case Some(title) =>

        val titleId = title.titleID

        val url = db.gamma.hs

        val rootDir = new File(s"downloads/${title.folder()}")
        rootDir.mkdirs()

        new File(rootDir, "title.cert").writeBytes(db.alpha.hb)

        val tmdOutputFile = new File(rootDir, "title.tmd")
        tmdOutputFile.download(new URL(s"$url/${titleId}/tmd"))

        val titleMetaData = TitleMetaData(tmdOutputFile.readBytes())

        if (title.isPatch())
          new File(rootDir, "title.tik").download(new URL(s"$url/${titleId}/cetk"))
        else
          new File(rootDir, "title.tik").writeBytes(TitleTicket.create(titleKey, db.beta.hb, titleMetaData).payload)

        val chunks = titleMetaData.contentIterator().toSeq

        val max = chunks.foldLeft(0L) {
          case (size, content) =>
            size + content.size()
        }

        println(s"""Downloading ${chunks.size} chunks into "${rootDir.getCanonicalPath}"""")

        implicit val progressBar = Some(ProgressBar(max))

        chunks.foreach {
          case content =>

            val outputContentFile = new File(rootDir, s"${content.filenameBase()}.app")

            if (!outputContentFile.exists() || outputContentFile.length() != content.size()) {
              outputContentFile.download(new URL(s"$url/${titleId}/${content.filenameBase()}"))
            } else {
              progressBar.foreach {
                _.add(content.size(), true)
              }
            }

            try {
              new File(rootDir, content.filenameBase() + ".h3").download(new URL(s"$url/${titleId}/${content.filenameBase()}.h3"))(None)
            } catch {
              case _: Exception =>
            }
        }

      case None =>
        println("This title can't be downloaded")
        System.exit(-1)
    }


  }


}
