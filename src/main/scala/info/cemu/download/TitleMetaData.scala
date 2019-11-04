package info.cemu.download

import java.io.{File}

import info.cemu.download.util.IO._

case class TitleMetaData(payload : Array[Byte]) extends Payload(payload) {

  def version(): Byte = getByte(0x180)

  def titleVersion() : Short = getShort(0x1DC)

  def contentCount() : Short = getShort(0x1DE)

  def issuer() : String = getString(0x140, 64)

  def titleId() : Array[Byte] = getBytes(0x18C, 8)

  def content(index : Int) : Content =
    Content(payload, 0xB04 + 0x30 * index)

  def contentIterator() : Iterator[Content] = new Iterator[Content] {
    var position : Int = 0
    val max : Int = contentCount()
    override def hasNext: Boolean = position < max

    override def next(): Content = {
      val result = content(position)
      position += 1
      result
    }
  }

}


object TitleMetaData {

  def apply(file : File) : TitleMetaData =
    TitleMetaData(file.readBytes())

}