package info.cemu.download.util

import java.io.{BufferedInputStream, BufferedOutputStream, File, FileInputStream, FileOutputStream, InputStream, OutputStream, RandomAccessFile}
import java.net.URL

import org.apache.commons.io.IOUtils
import java.security.{DigestInputStream, MessageDigest}

import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import Types._

object IO {

  implicit class URLExtension(url : URL) {


    def inputStream() : InputStream =
      new BufferedInputStream(url.openStream())

  }

  implicit class FileExtension(file: File) {

    def makeDirectories(): Boolean = {
      val parent = file.getParentFile
      if (parent != null)
        parent.mkdirs()
      else
        true
    }

    def randomAccess(mode: String = "r"): RandomAccessFile =
      new RandomAccessFile(file, mode)

    def inputStream(): InputStream =
      new FileInputStream(file)

    def outputStream(): OutputStream =
      new BufferedOutputStream(new FileOutputStream(file))

    def readBytes(): Array[Byte] = {
      val fis = new FileInputStream(file)
      val content = new Array[Byte](file.length().toInt)
      IOUtils.readFully(fis, content)
      content
    }

    def SHA1(): Array[Byte] = {
      val digest = MessageDigest.getInstance("SHA1")
      val inputStream = new BufferedInputStream(new FileInputStream(file))
      val digestInputStream = new DigestInputStream(inputStream, digest)
      val buffer = new Array[Byte](1024 * 1024)
      while (digestInputStream.read(buffer) > -1) {
      }
      val result = digestInputStream.getMessageDigest
      digestInputStream.close
      val resultingDigest = result.digest()
      resultingDigest
    }

  }

  def resourceToFile(name: String)(implicit rootPath: File): File = {
    val result = getClass.getClassLoader.getResource(name)
    if (result == null)
      new File(rootPath, name)
    else
      new File(result.toURI)
  }

  def SHA1(input: Array[Byte], length: Int, output: Array[Byte]): Unit = {
    val digest = MessageDigest.getInstance("SHA1")
    digest.update(input, 0, length)
    output.copy(digest.digest(), 0, 0x14)
  }

  def encrypt(input: Array[Byte], inputOffset: Int, inputLength: Int, output: Array[Byte], outputOffset: Int, key: Array[Byte], iv: Array[Byte], mode: Int): Int = {
    val initVector = new IvParameterSpec(iv)
    val secretKeySpec = new SecretKeySpec(key, "AES")
    val cipher = Cipher.getInstance("AES/CBC/NoPadding")
    cipher.init(mode, secretKeySpec, initVector)
    cipher.doFinal(input, inputOffset, inputLength, output, outputOffset)
  }

  def decrypt(inputOutput: Array[Byte], key: Array[Byte], iv: Array[Byte]): Int =
    encrypt(inputOutput, 0, inputOutput.length, inputOutput, 0, key, iv, Cipher.DECRYPT_MODE)

}
