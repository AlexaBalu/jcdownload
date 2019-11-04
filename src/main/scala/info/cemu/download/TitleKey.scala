package info.cemu.download

import java.io.File
import info.cemu.download.util.IO._

case class TitleKey(payload : Array[Byte]) extends Payload(payload) {

  def encryptedTitleKey() : Array[Byte] = getBytes(0x1BF, 16)


}

object TitleKey {

  def apply(file : File) : TitleKey =
    TitleKey(file.readBytes())

}