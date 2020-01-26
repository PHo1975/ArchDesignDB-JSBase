package clientbase.connection

import definition.data.OwnerReference

import scala.collection.mutable
import scala.scalajs.js.typedarray._

/**
*  manages Arrays to send Binary Data
*/
object TransferBuffersMap {
  val bufferMap: mutable.Map[Int, Array[Byte]] =collection.mutable.Map[Int,Array[Byte]]()

  def getArrayForSize(size:Int): Array[Byte] =bufferMap.getOrElseUpdate(size,new Array[Byte](size+17))

  def createArrayBuffer(blockTyp:Int,blockInst:Int,ownerReference: OwnerReference,data:Array[Byte]): ArrayBuffer = {
    var pos=0
    val array: Array[Byte] =getArrayForSize(data.length)

    def writeInt(v: Int): Unit =    {
      array(pos)=((v >>> 24) & 255).toByte
      array(pos+1)=((v >>> 16) & 255).toByte
      array(pos+2)=((v >>> 8) & 255).toByte
      array(pos+3)=((v >>> 0) & 255).toByte
      pos += 4
    }
    def writeByte(v:Byte):Unit= {
      array(pos)=v
      pos += 1
    }

    writeInt(blockTyp)
    writeInt(blockInst)
    writeByte(ownerReference.ownerField)
    writeInt(ownerReference.ownerRef.typ)
    writeInt(ownerReference.ownerRef.instance)
    data.copyToArray(array,17,data.length)
    println("TransBuffer data :"+array.mkString("|"))
    array.toTypedArray.buffer
  }
}
