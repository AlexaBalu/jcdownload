package info.cemu.download

import java.io.File

import info.cemu.download.util.{IO, ProgressBar}
import info.cemu.download.util.Types._
import info.cemu.download.util.IO._

object Decrypt {
  def main(args: Array[String]): Unit = {

    if (args.length < 2)
      throw new RuntimeException("java -jar jcdecrypt.jar <common key> <content path>")

    implicit val rootPath = new File(args(1))
    implicit val commonKey = args(0).fromHexToBytes()

    val tmd = TitleMetaData(IO.resourceToFile("title.tmd"))

    val tik = TitleTicket(IO.resourceToFile("title.tik"))

    val index = FST(tmd, tik)

    val parts = index.sortedEntries().toSeq

    val (cnt, max) = parts.flatMap(_._2).foldLeft[(Int, Long)]((0, 0L)) {
      case ((cnt: Int, current: Long), right: FEntry) =>
        (cnt + 1, current + right.getFileLength())
    }

    println(s"""Extracting ${cnt} files""")

    implicit val progress = Some(ProgressBar(max))

    parts.foreach{
      case (file, entries) =>
        val input = file.randomAccess()
        try {
          entries.foreach {
            case entry =>
              entry.extractFile(input)
          }
        } finally {
          input.close()
        }
    }

  }
}
