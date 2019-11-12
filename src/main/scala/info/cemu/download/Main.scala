package info.cemu.download

import java.io.{File, RandomAccessFile}
import java.net.URL

import info.cemu.download.util.{IO, ProgressBar}
import info.cemu.download.util.Types._
import info.cemu.download.util.IO._

object Main {

  def main(args: Array[String]): Unit = {

    if (args.length < 2)
      throw new RuntimeException("java -jar jcdownload.jar <common key> <title key>")

    implicit val commonKey = args(0).hb

    val db = Database(commonKey)

    val titleKey = args(1).hb

    db.findTitle(titleKey) match {
      case Some(title) =>

        implicit var progressBar : Option[ProgressBar] =  None

        val titleId = title.titleID

        val url = db.gamma.hs

        implicit val rootDir = new File(s"downloads/${title.folder()}")
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

        var parts : Option[Map[File, Seq[FEntry]]] = None

        def processContainer(index : Int, contentFile : File, contentFileDescriptor : RandomAccessFile) : Unit = {
          if (index == 0) {
            val tik = TitleTicket(IO.resourceToFile("title.tik"))
            val index = FST(titleMetaData, tik)
            val toExtract = index.sortedEntries().toMap
            val (cnt, max) = toExtract.toSeq.flatMap(_._2).foldLeft[(Int, Long)]((0, 0L)) {
              case ((cnt: Int, current: Long), right: FEntry) =>
                (cnt + 1, current + right.getFileLength())
            }
            println(s"""Extracting ${cnt} files during that process""")
            progressBar = Some(ProgressBar(max))
            parts = Some(toExtract)
          } else  {
            parts.foreach{
              case toExtract =>
                toExtract.get(contentFile).foreach{
                  case entries =>
                    entries.foreach {
                      case entry =>
                        entry.extractFile(contentFileDescriptor)(None)
                    }
                }
            }
          }
        }

        chunks.zipWithIndex.foreach {
          case (content, index) =>
            val contentFile = new File(rootDir, s"${content.filenameBase()}.app")
            val contentFileDescriptor = contentFile.randomAccess()
            try {
              if (!contentFile.exists() || contentFile.length() != content.size()) {
                contentFileDescriptor.download(new URL(s"$url/${titleId}/${content.filenameBase()}"))
                processContainer(index, contentFile, contentFileDescriptor)
              } else {
                progressBar.foreach {
                  _.add(content.size())
                }
                processContainer(index, contentFile, contentFileDescriptor)
              }
            } finally {
              contentFileDescriptor.close()
            }
            try {
              new File(rootDir, content.filenameBase() + ".h3").download(new URL(s"$url/${titleId}/${content.filenameBase()}.h3"))
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
