package info.cemu.download

import java.io.{File, IOException, RandomAccessFile}
import java.net.URL

import info.cemu.download.util.{IO, ProgressBar}
import info.cemu.download.util.Types._
import info.cemu.download.util.IO._

object Main {


  def main(args: Array[String]): Unit = {

    val arguments = Arguments(args)

    if (arguments.count() < 2)
      throw new RuntimeException("java -jar jcdownload.jar <COMMON KEY> <TITLE KEY or FOLDER or TITLE KEY FILE>")

    implicit val commonKey = arguments.getCommonKey()

    val db = Database(commonKey)

    val titleKey = arguments.getTitleKey()

    db.findTitle(titleKey) match {
      case Some(title) =>

        implicit var progressBar: Option[ProgressBar] = None

        val titleId = title.titleID

        val url = db.gamma.hs

        implicit val rootDir = arguments.getRootFolder().getOrElse(
          new File(s"downloads/${title.folder()}")
        )
        rootDir.mkdirs()

        havingAnyOf("title.cert", "cert") {
          new File(rootDir, _)
        }.writeBytes(db.alpha.hb)

        val tmdOutputFile = havingAnyOf("title.tmd", "tmd") {
          new File(rootDir, _)
        }

        tmdOutputFile.download(new URL(s"$url/${titleId}/tmd"))

        val titleMetaData = TitleMetaData(tmdOutputFile.readBytes())

        val ticketFile = havingAnyOf("title.tik", "cetk") {
          new File(rootDir, _)
        }

        if (title.isPatch())
          ticketFile.download(new URL(s"$url/${titleId}/cetk"))
        else
          ticketFile.writeBytes(TitleTicket.create(titleKey, db.beta.hb, titleMetaData).payload)

        val chunks = titleMetaData.contentIterator().toSeq

        val max = chunks.foldLeft(0L) {
          case (size, content) =>
            size + content.size()
        }

        println(s"""Downloading ${chunks.size} chunks of ${max.toDisplaySize()} into "${rootDir.getCanonicalPath}"""")

        var parts: Option[Map[File, Seq[FEntry]]] = None

        def processContainer(index: Int, contentFile: File, contentFileDescriptor: RandomAccessFile, contentSize: Long): Unit = {
          progressBar.foreach {
            _.setCurrentChunk(index + 1)
          }
          if (index == 0) {
            val tik = havingAnyOf("title.tik", "cetk") {
              filename =>
                TitleTicket(IO.resourceToFile(filename))
            }
            val index = FST(titleMetaData, tik)
            val toExtract = index.sortedEntries().toMap

            /*index.sortedEntries().foreach{
              case (file, entries) =>
                println(s"Container ${file.getName}")
                entries.foreach{
                  case entry =>
                    println(s"- %s f:%x t:%x %d %s %d".format(
                      entry.getFullPath(), entry.getFlags(), entry.getType(), entry.getFileOffset(),
                      entry.getFileLength().toDisplaySize(), entry.getFileLength()))
                }
            }*/

            val (cnt, _) = toExtract.toSeq.flatMap(_._2).foldLeft[(Int, Long)]((0, 0L)) {
              case ((cnt: Int, current: Long), right: FEntry) =>
                (cnt + 1, current + right.getFileLength())
            }
            println(s"""Extracting and decrypting ${cnt} files during that process""")
            val pb = ProgressBar(max)
            pb.setChunksCount(chunks.length)
            pb.add(contentSize)
            progressBar = Some(pb)
            parts = Some(toExtract)
          } else {
            parts.foreach {
              case toExtract =>
                toExtract.get(contentFile).foreach {
                  case entries =>
                    progressBar.foreach {
                      _.setFilesCount(entries.length)
                    }
                    entries.zipWithIndex.foreach {
                      case (entry, index) =>
                        entry.extractFile(contentFileDescriptor)(None)
                        progressBar.foreach {
                          _.setCurrentFile(index + 1)
                        }
                    }
                }
            }
          }
        }

        def tryToDownloadAndUnpack(contentFile: File, content: Content, index: Int, tries: Int): Unit = {
          val position = progressBar.map(_.get())
          try {
            val contentFileDescriptor = contentFile.randomAccess()
            try {
              if (!contentFile.exists() || (contentFile.length() != content.size())) {
                if (!contentFileDescriptor.resume(new URL(s"$url/${titleId}/${content.filenameBase()}"), content.size()))
                  throw new RuntimeException(s"""Failed to successfully download "${content.filenameBase()}" container""")
                processContainer(index, contentFile, contentFileDescriptor, content.size())
              } else {
                progressBar.foreach {
                  _.add(content.size(), true)
                }
                processContainer(index, contentFile, contentFileDescriptor, content.size())
              }
            } finally {
              contentFileDescriptor.close()
            }
          } catch {
            case e: Exception =>
              if (tries > 0) {
                if (!e.isInstanceOf[IOException]) {
                  contentFile.delete() // try again from scratch
                }
                progressBar.foreach {
                  bar =>
                    position.foreach { // fall back to old progress
                      bar.set
                    }
                }
                progressBar.foreach(
                  _.markFailure()
                )
                tryToDownloadAndUnpack(contentFile, content, index, tries - 1)
              } else {
                throw e
              }
          }
        }

        chunks.zipWithIndex.foreach {
          case (content, index) =>
            val contentFile =
              havingAnyOf(s"${content.filenameBase()}.app", content.filenameBase()) {
                new File(rootDir, _)
              }
            tryToDownloadAndUnpack(contentFile, content, index, 3)
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

  case class Arguments(args: Array[String]) {

    def count(): Int = args.length

    def getCommonKey(): Array[Byte] = args(0).trim.hb

    def getRootFolder(): Option[File] = {
      val value = args(1).trim
      val content = new File(value)
      if (content.isDirectory) {
        Some(content)
      } else if (content.isFile()) {
        Some(content.getParentFile)
      } else {
        None
      }
    }

    def getTitleKey(): Array[Byte] = {
      val value = args(1).trim
      val content = new File(value)
      if (content.isDirectory) {
        implicit val root = getRootFolder().getOrElse(
          throw new RuntimeException("Failed to determine root directory")
        )
        val tik = havingAnyOf("title.tik", "cetk") {
          filename =>
            TitleTicket(IO.resourceToFile(filename)(content))
        }
        tik.encryptedTitleKey()
      } else if (content.isFile()) {
        val tik = TitleTicket(IO.resourceToFile(content.getName)(content.getParentFile))
        tik.encryptedTitleKey()
      } else {
        value.hb
      }
    }

  }


}
