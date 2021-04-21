package clientbase.connection
import java.io.InputStream
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, Uint8Array}

class ArrayBufferInputStream(val buffer: ArrayBuffer, val offset: Int,
                             val length: Int) extends InputStream {

  /** Convenience constructor. Strictly equivalent to
   *  {{new ArrayBufferInputStream(buffer, 0, buffer.byteLength)}
   */
  def this(buffer: ArrayBuffer) = this(buffer, 0, buffer.byteLength)

  private val uintView = new Uint8Array(buffer, offset, length)
  private val byteView = new Int8Array(buffer, offset, length)

  /** Used to persist [[pos]] when mark is called */
  protected var mark: Int = 0

  /** Next byte to read in the buffer (after adding offset).
   *
   *  Use [[skip]] to update (protects from overrun and moving backwards).
   */
  @inline def pos: Int = _pos
  @inline protected def pos_=(x: Int): Unit = _pos = x
  private[this] var _pos: Int = 0

  override def available(): Int = length - pos
  override def mark(readlimit: Int): Unit = { mark = pos }
  override def markSupported(): Boolean = true
  def read(): Int = {
    if (pos < length) {
      val res = uintView(pos)
      pos += 1
      res
    } else -1
  }

  override def read(b: Array[Byte], off: Int, reqLen: Int): Int = {
    if (off < 0 || reqLen < 0 || reqLen > b.length - off)
      throw new IndexOutOfBoundsException

    val len = Math.min(reqLen, length - pos)

    if (reqLen == 0)
      0 // 0 requested, 0 returned
    else if (len == 0)
      -1 // nothing to read at all
    else {
      var i = 0
      while (i < len) {
        b(i + off) = byteView(pos + i)
        i += 1
      }
      pos += len
      len
    }
  }

  override def reset(): Unit = { pos = mark }

  /** Skips a given number of bytes. Always skips the maximum number possible */
  override def skip(n: Long): Long = {
    val k = Math.max(0, Math.min(n, length - pos)).toInt
    pos += k
    k.toLong
  }

}

