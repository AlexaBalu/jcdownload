package info.cemu.download

import java.io.File

import info.cemu.download.util.IO
import info.cemu.download.util.IO._
import info.cemu.download.util.Types._
import javax.crypto.Cipher

import scala.jdk.CollectionConverters._

case class Title(titleID: String, titleHash: String, name: String, region: String, ticket: Boolean) {

  def isPatch() : Boolean = titleID.charAt(7) == 'e'

  def isDLC() : Boolean = titleID.charAt(7) == 'c'

  def isDemo() : Boolean = titleID.charAt(7) == '2'

  private def safeName() : String = if (name != null) name else titleID

  private def safeRegion() : String = if (region != null) region else "UNKNOWN"

  def folder() : File = {
    val stripUnsafe = safeName().replaceAll("&", " and ").replaceAll("\\+", " plus").replaceAll("[^a-z^A-Z^0-9^\\s^-^_]+", "").replaceAll("\\s+", " ").trim
    val folderName = "[" + safeRegion() + "] " + (
      if (isPatch()) "(PATCH) "
      else if (isDLC()) "(DLC) "
      else if (isDemo()) "(DEMO) "
      else ""
    ) + (if (stripUnsafe.length <= 3) titleID else stripUnsafe)
    new File(folderName)
  }
}

case class Database(alpha: String, beta: String, gamma : String, index: java.util.List[Title]) {

  def findTitle(key: Array[Byte]): Option[Title] = {
    val filter = IO.SHA256(key).bh
    index.asScala.find {
      case title =>
        filter.equals(title.titleHash)
    }
  }

}

object Database {

  def apply(key: Array[Byte]): Database = {
    val reference = resourceToBytes("6c6f676f2e737667".hs)
    val data = "283f6d292e2a3f3c6d657461646174613e282e2a3f293c2f6d657461646174613e2e2a".hs.r
    val content = data.findFirstMatchIn(new String(reference)).map(
      _.group(1).hb
    ).getOrElse(throw new RuntimeException())
    IO.encrypt(content, 0, content.length, content, 0, key, key.reverse, Cipher.DECRYPT_MODE, false)
    IO.deserialize(IO.uncompress(content), classOf[Database])
  }

}
