package info.cemu.download

import java.io.{File, RandomAccessFile}

import javax.crypto.Cipher
import util.IO._
import util.Types._
import FEntry._
import info.cemu.download.util.ProgressBar

case class FEntry(payload: Array[Byte], offset: Int,
                  nameOffset: Int, parent: FST, fullPath: String)(implicit rootPath: File) extends Payload(payload, offset) {

  def getName(): String = {
    val names = new Payload(payload, nameOffset)
    val info = getInt(0)
    val offset = (info & 0x00FFFFFF)
    names.getString(offset, 128)
  }

  def isFolder(): Boolean = (getType() & 1) != 0

  def isExtractable(): Boolean = (getType() & 0x80) == 0

  protected def isHashed(): Boolean = (getFlags() & 0x440) != 0

  def getFullPath(): String = fullPath

  def getContentID(): Short = getShort(0x0E)

  def getFlags(): Short = getShort(0x0C)

  def getContainer(): File = resourceToFile(parent.tmd.content(getContentID()).filenameBase() + ".app")

  def getType(): Int = ((getInt(0) & 0xFF000000) >>> 24)

  def getTypeName(): Int = getInt(0)

  def getParentOffset(): Int = getInt(0x04)

  def getNextOffset(): Int = getInt(0x08)

  def getFileOffset(): Long = {
    val offset = Integer.toUnsignedLong(getInt(0x04))
    if ((getFlags() & 4) == 0)
      offset << 5
    else
      offset
  }

  def getFileLength(): Long = Integer.toUnsignedLong(getInt(0x08))

  def extractFile(in: RandomAccessFile)(implicit progress: Option[ProgressBar]): Unit = {

    if (!isExtractable()) {
      // skip
    }
    else if (isHashed()) {
      extractHashedFile(in)
    }
    else {

      val outputFilename = resourceToFile(getFullPath())
      var bytesLeftToWrite: Long = getFileLength()

      if (!outputFilename.exists() || outputFilename.length() != bytesLeftToWrite) {

        outputFilename.makeDirectories()
        val output = outputFilename.outputStream()

        val fileOffset: Long = getFileOffset()

        val alignedReadOffset: Long = (fileOffset / BLOCK_SIZE) * BLOCK_SIZE
        var initialStoreOffset: Long = fileOffset - alignedReadOffset

        val IV = byteArray(16)
        val swapIV = byteArray(16)
        IV(0) = ((getContentID() >> 8) & 0xFF).toByte
        IV(1) = getContentID().toByte

        var bytesToWrite: Long = BLOCK_SIZE

        if ((initialStoreOffset + bytesLeftToWrite) > bytesToWrite)
          bytesToWrite = bytesToWrite - initialStoreOffset

        in.seek(alignedReadOffset)

        while (bytesLeftToWrite > 0) {

          if (bytesToWrite > bytesLeftToWrite)
            bytesToWrite = bytesLeftToWrite

          in.read(blockBuffer, 0, BLOCK_SIZE)

          swapIV.copy(blockBuffer, BLOCK_SIZE - 16, 16)

          encrypt(blockBuffer, 0, BLOCK_SIZE, blockBuffer, 0, parent.decryptedKey, IV, Cipher.DECRYPT_MODE)

          IV.copy(swapIV, 0, swapIV.length)

          output.write(blockBuffer, initialStoreOffset.toInt, bytesToWrite.toInt)
          progress.foreach {
            _.add(bytesToWrite)
          }

          bytesLeftToWrite -= bytesToWrite

          bytesToWrite = BLOCK_SIZE
          initialStoreOffset = 0
        }

        output.close()
      } else {
        progress.foreach{
          _.add(bytesLeftToWrite)
        }
      }
    }
  }

  protected def extractHashedFile(in: RandomAccessFile)(implicit progress: Option[ProgressBar]): Unit = {

    val outputFilename = resourceToFile(getFullPath())

    var Size: Long = getFileLength()

    if (!outputFilename.exists() || outputFilename.length() != Size) {

      val HASH_BLOCK_SIZE = 0xFC00
      val SHA_DIGEST_LENGTH = 0x14
      val HASHES_BLOCK_SIZE = 0x400

      outputFilename.makeDirectories()
      val output = outputFilename.outputStream()

      val fileOffset: Long = getFileOffset()

      val ContentID: Byte = getContentID().toByte

      val hash = byteArray(SHA_DIGEST_LENGTH)
      val H0 = byteArray(SHA_DIGEST_LENGTH)
      val Hashes = byteArray(HASHES_BLOCK_SIZE)
      val IV = byteArray(16)

      var Wrote: Long = 0
      var WriteSize: Long = HASH_BLOCK_SIZE // Hash block size
      var Block = (fileOffset / HASH_BLOCK_SIZE) & 0xF

      val alignedReadOffset = fileOffset / HASH_BLOCK_SIZE * HASHED_BLOCK_SIZE
      var initialStoreOffset = fileOffset - (fileOffset / HASH_BLOCK_SIZE * HASH_BLOCK_SIZE)

      if (initialStoreOffset + Size > WriteSize)
        WriteSize = WriteSize - initialStoreOffset

      in.seek(alignedReadOffset)

      while (Size > 0) {

        if (WriteSize > Size)
          WriteSize = Size

        in.read(hashedBuffer, 0, HASHED_BLOCK_SIZE)

        IV.fill(0)
        IV(0) = ((ContentID >> 8) & 0xFF).toByte
        IV(1) = ContentID.toByte

        encrypt(hashedBuffer, 0, HASHES_BLOCK_SIZE, Hashes, 0, parent.decryptedKey, IV, Cipher.DECRYPT_MODE)

        H0.copy(Hashes, (SHA_DIGEST_LENGTH * Block).toInt, 20)
        IV.copy(Hashes, (SHA_DIGEST_LENGTH * Block).toInt, 16)

        if (Block == 0)
          IV(1) = (IV(1) ^ ContentID).toByte

        encrypt(hashedBuffer, HASHES_BLOCK_SIZE, HASH_BLOCK_SIZE, hashedBuffer, 0, parent.decryptedKey, IV, Cipher.DECRYPT_MODE)

        SHA1(hashedBuffer, HASH_BLOCK_SIZE, hash)

        if (Block == 0)
          hash(1) = (hash(1) ^ ContentID).toByte

        if (!java.util.Arrays.equals(hash, H0))
          throw new RuntimeException(s"Wrong H0 hash value, failed to extract ${getFullPath()}")

        output.write(hashedBuffer, initialStoreOffset.toInt, WriteSize.toInt)
        progress.foreach{
          _.add(WriteSize)
        }


        Size -= WriteSize
        Wrote += WriteSize

        Block = ((Block + 1) % 16)

        WriteSize = HASH_BLOCK_SIZE
        initialStoreOffset = 0

      }

      output.close()

    } else {
      progress.foreach{
        _.add(Size)
      }
    }
  }
}

object FEntry {
  val BLOCK_SIZE: Int = 0x8000
  val blockBuffer = byteArray(BLOCK_SIZE)

  val HASHED_BLOCK_SIZE = 0x10000
  val hashedBuffer = byteArray(HASHED_BLOCK_SIZE)
}