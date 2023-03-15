package info.cemu.download.util

import java.nio.ByteBuffer

object Types {

  def byteArray(size: Int): Array[Byte] = Array.ofDim[Byte](size)

  def byteArrayOf(elements: Int*): Array[Byte] = elements.map(_.toByte).toArray[Byte]

  implicit class StringType(value: String) {
    def fromHexToBytes(): Array[Byte] = {
      val data = new Array[Byte](value.length / 2)
      var i = 0
      while (i < value.length) {
        data(i / 2) = Integer.decode("0x" + value.charAt(i) + value.charAt(i + 1)).byteValue
        i += 2
      }
      data
    }

    def fromHexToString(): String = new String(value.fromHexToBytes())

    def hs: String = fromHexToString()

    def hb: Array[Byte] = fromHexToBytes()
  }

  implicit class ByteArrayType(value: Array[Byte]) {

    def fill(newValue: Byte): Array[Byte] = {
      var i = 0
      val len = value.length
      while (i < len) {
        value(i) = newValue
        i += 1
      }
      value
    }

    def cloned(): Array[Byte] =
      java.util.Arrays.copyOf(value, value.length)

    def copy(source: Array[Byte], offset: Int, length: Int): Unit =
      Array.copy(source, offset, value, 0, length)

    def len(): Int = {
      var i = 0
      while (i < value.length && value(i) != 0) {
        i += 1
      }
      i
    }

    def toHexString(): String = value.map(v => "%02x".format(v)).mkString

    def bh: String = toHexString()

    def padRight(size: Int): Array[Byte] = {
      val result = byteArray(value.length + size)
      Array.copy(value, 0, result, 0, value.length)
      result
    }
  }

  implicit class ByteBufferType(buffer: ByteBuffer) {

    def readString(offset: Int, maxLength: Int): String = {
      var len = 0
      while (buffer.get(offset + len) != 0) {
        len += 1
      }
      val dest = readBytes(offset, len)
      new String(dest, 0, dest.length /*.len()*/ , "ASCII")
    }

    def readBytes(offset: Int, length: Int, size: Int = -1): Array[Byte] = {
      val dest = byteArray(if (size == -1) length else size)
      buffer.position(offset)
      buffer.get(dest, 0, length)
      buffer.position(0)
      dest
    }


  }

  implicit class Units(value: Long) {


    def toDisplaySize(si: Boolean = true, space: Boolean = false): String = {
      val unit = if (si) 1000 else 1024
      if (value < unit)
        value + (if (space) " " else "") + "B"
      else {
        val exp = (Math.log(value) / Math.log(unit)).toInt
        val pre = (if (si) "kMGTPE" else "KMGTPE").charAt(exp - 1) + (if (si) "" else "i")
        "%.2f".format(value / Math.pow(unit, exp)) + (if (space) " " else "") + "%sB".format(pre);
      }
    }

    def toDisplayTime(): String = {
      val second = (value / 1000) % 60
      val minute = (value / (1000 * 60)) % 60
      val hour = (value / (1000 * 60 * 60)) % 24
      "%d:%02d:%02d".format(hour, minute, second)
    }

  }


}
