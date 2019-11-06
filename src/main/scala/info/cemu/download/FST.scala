package info.cemu.download

import info.cemu.download.util.IO
import info.cemu.download.util.Types._
import java.io.File
import info.cemu.download.util.IO._

case class FST(payload: Array[Byte], tmd: TitleMetaData, tik: TitleKey,
               decryptedKey: Array[Byte])(implicit rootPath : File) extends Payload(payload) {

  if (magicValue() != 0x46535400)
    throw new RuntimeException("Invalid index, control value not matched after decryption")

  def magicValue() = getInt(0)

  def getInfoEntriesCount() = getInt(0x08)

  def getEntriesCount() = getInt(0x20 + getInfoEntriesCount() * 0x20 + 8)

  def getEntry(index: Int, fullPath: String = ""): FEntry =
    FEntry(payload, (0x20 + getInfoEntriesCount() * 0x20) + index * 0x10,
      0x20 + getInfoEntriesCount() * 0x20 + getEntriesCount() * 0x10, this, fullPath)

  def entriesIterator(): Iterator[FEntry] = new Iterator[FEntry] {
    var i: Int = 1
    val entriesCount: Int = getEntriesCount()
    val entry = Array.ofDim[Int](16)
    val lEntry = Array.ofDim[Int](16)
    var level: Int = 0
    var current: FEntry = null

    override def hasNext: Boolean = {
      if (i < entriesCount) {
        if (current == null) {
          while (level >= 1 && lEntry(level - 1) == i) {
            level -= 1
          }
          val givenEntry = getEntry(i)
          if (givenEntry.isFolder()) {
            entry(level) = i
            lEntry(level) = givenEntry.getNextOffset()
            level += 1
            i += 1
            current = next()
          } else {
            val path = ((0 until level).map(i => getEntry(entry(i)).getName())
              :+ givenEntry.getName()).mkString(File.separator)
            val result = getEntry(i, path)
            i += 1
            current = result
          }
        }
        current != null
      } else
        false
    }

    override def next(): FEntry = {
      hasNext
      val result = current
      current = null
      result
    }
  }
}


object FST {

  def decryptedTitleKey(tmd: TitleMetaData, tik: TitleKey, commonKey : Array[Byte]): Array[Byte] = {
    val encryptedKey = tik.encryptedTitleKey().cloned()
    IO.decrypt(encryptedKey, commonKey, tmd.titleId().padRight(8))
    encryptedKey
  }

  def apply(tmd: TitleMetaData, tik: TitleKey)(implicit commonKey : Array[Byte], rootPath : File): FST = {

    val indexContent = tmd.content(0)

    val file = IO.resourceToFile(indexContent.filename())

    val encryptedContent = file.readBytes()

    if (encryptedContent.length != indexContent.size())
      throw new RuntimeException(s"Wrong index size, expected ${indexContent.size()}, read only ${encryptedContent.length}")

    val decryptedKey = decryptedTitleKey(tmd, tik, commonKey)

    val newSize = IO.decrypt(
      encryptedContent,
      decryptedKey,
      byteArray(16))

    if (newSize != indexContent.size())
      throw new RuntimeException(s"Size is different after decompression, was ${encryptedContent.length}, is ${newSize}")

    FST(encryptedContent, tmd, tik, decryptedKey)
  }

}