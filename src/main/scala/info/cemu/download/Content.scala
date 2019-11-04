package info.cemu.download

case class Content(payload : Array[Byte], offset : Int) extends Payload(payload, offset) {

  def id() = getInt(0)

  def index() = getShort(0x4)

  def contentType() = getShort(0x6)

  def size() = getLong(0x8)

  def sha2() = getBytes( 0x10, 0x14)

  def filename() = "%08x.app".format(id())

  def filenameBase() = "%08x".format(id())

}
