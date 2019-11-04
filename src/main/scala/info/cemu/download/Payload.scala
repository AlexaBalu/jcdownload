package info.cemu.download

import java.nio.ByteBuffer
import info.cemu.download.util.Types._

class Payload(payload : Array[Byte], baseOffset : Int = 0) {

  lazy val buffer = ByteBuffer.wrap(payload)

  def getByte(offset : Int) : Byte = buffer.get(baseOffset + offset)

  def getShort(offset : Int) : Short = buffer.getShort(baseOffset + offset)

  def getLong(offset : Int) : Long = buffer.getLong(baseOffset + offset)

  def getInt(offset : Int) : Int = buffer.getInt(baseOffset + offset)

  def getString(offset : Int, length : Int) : String = buffer.readString(baseOffset + offset, length)

  def getBytes(offset : Int, length : Int, totalSize : Int = -1) : Array[Byte] = buffer.readBytes(baseOffset + offset, length, totalSize)

}
