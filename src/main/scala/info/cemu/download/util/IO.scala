package info.cemu.download.util

import java.io.{BufferedInputStream, BufferedOutputStream, ByteArrayInputStream, ByteArrayOutputStream, File, FileInputStream, FileOutputStream, InputStream, InputStreamReader, OutputStream, RandomAccessFile}
import java.net.URL

import org.apache.commons.io.IOUtils
import java.security.{DigestInputStream, MessageDigest}
import java.util.zip.{DeflaterOutputStream, InflaterInputStream, InflaterOutputStream, ZipInputStream, ZipOutputStream}

import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}
import Types._
import com.google.gson.Gson

object IO {

  val buffer = byteArray(1024 * 1024)

  implicit class URLExtension(url: URL) {


    def inputStream(): InputStream =
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

    def writeBytes(content : Array[Byte]): Unit = {
      val output = new FileOutputStream(file)
      output.write(content)
      output.close()
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


    def download(input: URL)(implicit progressBar: Option[ProgressBar] = None): Boolean = {
      lazy val tmdOutput = file.outputStream()
      var readSize: Int = 0
      try {

        val stream = input.openStream()

        try {

          while ( {
            readSize = stream.read(buffer, 0, buffer.length);
            readSize > -1
          }) {
            tmdOutput.write(buffer, 0, readSize)
            progressBar.foreach {
              _.add(readSize)
            }
          }

          readSize != 0
        } catch {
          case _ : Exception =>
            false
        } finally {
          stream.close()
        }

      } finally {
        if (readSize != 0)
          tmdOutput.close()
      }
    }

  }

  def copy(in: InputStream, out: OutputStream): Unit = {
    val buffer = byteArray(1024 * 8)
    var i = 0
    while ( {
      i = in.read(buffer, 0, buffer.length);
      i != -1
    }) {
      out.write(buffer, 0, i)
    }
  }

  def resourceToBytes(name: String): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    copy(getClass.getClassLoader.getResourceAsStream(name), out)
    out.toByteArray
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

  def SHA256(input: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(input)

  def encrypt(input: Array[Byte], inputOffset: Int, inputLength: Int, output: Array[Byte], outputOffset: Int, key: Array[Byte], iv: Array[Byte], mode: Int, fixed: Boolean = true): Int = {
    val initVector = new IvParameterSpec(iv)
    val secretKeySpec = new SecretKeySpec(key, "AES")
    val cipher = Cipher.getInstance(if (fixed) "AES/CBC/NoPadding" else "AES/CTR/NoPadding")
    cipher.init(mode, secretKeySpec, initVector)
    cipher.doFinal(input, inputOffset, inputLength, output, outputOffset)
  }

  def decrypt(inputOutput: Array[Byte], key: Array[Byte], iv: Array[Byte]): Int =
    encrypt(inputOutput, 0, inputOutput.length, inputOutput, 0, key, iv, Cipher.DECRYPT_MODE)

  def deserialize[T](input: Array[Byte], clazz: Class[T]): T = {
    val in = new ByteArrayInputStream(input)
    val reader = new InputStreamReader(in, "UTF-8")
    new Gson().fromJson(reader, clazz)
  }

  def serialize(input: Any): String =
    new Gson().toJson(input)

  def compress(input: Array[Byte]): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val co = new DeflaterOutputStream(out)
    co.write(input, 0, input.length)
    co.close()
    out.toByteArray
  }

  def uncompress(input: Array[Byte]): Array[Byte] = {
    val out = new ByteArrayOutputStream()
    val in = new ByteArrayInputStream(input)
    val de = new InflaterOutputStream(out)
    copy(in, de)
    de.close()
    out.toByteArray
  }
}
