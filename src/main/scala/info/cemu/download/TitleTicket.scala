package info.cemu.download

import java.io.File
import java.nio.ByteBuffer
import java.util.Base64

import info.cemu.download.util.IO
import info.cemu.download.util.Types._
import info.cemu.download.util.IO._

case class TitleTicket(payload: Array[Byte]) extends Payload(payload) {

  def encryptedTitleKey(): Array[Byte] = getBytes(0x1BF, 16)

}

object TitleTicket {

  val TICKET_OFFSET = 0x140

  def apply(file: File): TitleTicket =
    TitleTicket(file.readBytes())

  def create(titleKey: Array[Byte], payload: Array[Byte], tmd: TitleMetaData): TitleTicket = {
    val output = ByteBuffer.wrap(payload)
    output.putShort(TICKET_OFFSET + 0xA6, tmd.titleVersion())
    output.position(TICKET_OFFSET + 0x9C)
    output.put(tmd.titleId())
    output.position(TICKET_OFFSET + 0x7F)
    output.put(titleKey)
    val ticket = TitleTicket(payload)
    patchDemo(ticket, tmd)
    patchDLC(ticket, tmd)
    ticket
  }

  def patchDemo(ticket: TitleTicket, tmd: TitleMetaData): Array[Byte] = {
    if (tmd.isDemo()) {
      val output = ByteBuffer.wrap(ticket.payload)
      output.position(TICKET_OFFSET + 0x124)
      output.put(byteArray(64))
    }
    ticket.payload
  }

  def patchDLC(ticket: TitleTicket, tmd: TitleMetaData): Array[Byte] = {
    if (tmd.isDCL()) {
      val output = ByteBuffer.wrap(ticket.payload)
      val patch = IO.uncompress(Base64.getDecoder.decode("eNpjYGQQYWBgWAPEIgwQNghoADEjELeAMTNE8D8BwEBjAABCdSH/"))
      output.position(TICKET_OFFSET + 0x164)
      output.put(patch)
    }
    ticket.payload
  }

}