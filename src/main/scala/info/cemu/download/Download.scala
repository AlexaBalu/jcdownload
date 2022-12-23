package info.cemu.download

import java.io.{File}
import java.net.URL

import info.cemu.download.util.{ProgressBar}
import info.cemu.download.util.Types._
import info.cemu.download.util.IO._

object Download {

  def main(args: Array[String]): Unit = {

    if (args.length < 2)
      throw new RuntimeException("java -cp jcdownload.jar info.cemu.download.Download <common key> <title key>")

    val db = Database(args(0).hb)

    val titleKey = args(1).hb

    db.findTitle(titleKey) match {
      case Some(title) =>

        val titleId = title.titleID

        val url = db.gamma.hs

        val rootDir = new File(s"downloads/${title.folder()}")
        rootDir.mkdirs()

        val cert = new File(rootDir, "title.cert")
        if (!cert.exists())
          cert.writeBytes(db.alpha.hb)

        val tmdOutputFile = new File(rootDir, "title.tmd")
        if (!tmdOutputFile.exists())
          tmdOutputFile.download(new URL(s"$url/${titleId}/tmd"))

        val titleMetaData = TitleMetaData(tmdOutputFile.readBytes())

        val file = new File(rootDir, "title.tik")

        if (!file.exists()) {
          if (title.isPatch())
            file.download(new URL(s"$url/${titleId}/cetk"))
          else
            file.writeBytes(TitleTicket.create(titleKey, db.beta.hb, titleMetaData).payload)
        }

        val chunks = titleMetaData.contentIterator().toSeq

        val max = chunks.foldLeft(0L) {
          case (size, content) =>
            size + content.size()
        }

        println(s"""Downloading ${chunks.size} chunks into "${rootDir.getCanonicalPath}"""")

        implicit val progressBar = Some(ProgressBar(max))

        progressBar.foreach{
          _.setChunksCount(chunks.length)
        }

        chunks.zipWithIndex.foreach {
          case (content, index) =>

            val outputContentFile = new File(rootDir, s"${content.filenameBase()}.app")

            progressBar.foreach {

              p =>
                p.setCurrentChunk(index + 1)
                p.resetFilesCount()
            }

            val contentFileDescriptor = outputContentFile.randomAccess()

            if (!outputContentFile.exists() || outputContentFile.length() < content.size()) {
              contentFileDescriptor.resume(new URL(s"$url/${titleId}/${content.filenameBase()}"), content.size())
            } else {
              progressBar.foreach {
                _.add(content.size(), true)
              }
            }

            try {
              val h3 = new File(rootDir, content.filenameBase() + ".h3")
              if (!h3.exists()) {
                h3.download(new URL(s"$url/${titleId}/${content.filenameBase()}.h3"))(None)
              }
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
