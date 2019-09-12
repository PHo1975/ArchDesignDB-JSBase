package clientbase.connection

import definition.typ.AllClasses
import util._

import scala.collection.immutable.IndexedSeq
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by Peter on 08.07.2015.
  */

class ColumnInfo(val ix: Int, val width: Int) {
  override def toString: String = ix.toString + "," + width
}


object TableSettings {
  protected val columnData: mutable.HashMap[Int, Seq[ColumnInfo]] = collection.mutable.HashMap[Int, Seq[ColumnInfo]]()

  def load(settingsString: String): Unit = {
    settingsString.trim() match {
      case "" =>
      case BarSplitList(blist) => for (b <- blist) b match {
        case ColonSplit(StrToInt(typ), setting) => setting match {
          case SemicolonSplitList(secList) =>
            val myClass = AllClasses.get.getClassByID(typ)
            val colBuffer = ArrayBuffer[ColumnInfo]()
            for (s <- secList) s match {
              case CommaSplit(StrToInt(ix), StrToInt(width)) => if (myClass.fieldSetting(ix).visible) colBuffer += new ColumnInfo(ix, width)
              case other => Log.e("unknown columnSetting for type " + typ + " :" + other + " in " + setting)
            }
            columnData(typ) = colBuffer
          case other => Log.e("unknown tableSetting for type " + typ + " :" + other)
        }
        case other => Log.e("unknown tableSetting " + other)
      }
      case other => Log.e("unknown tableSettings " + other)
    }
  }

  def getColumnData(typ: Int): Seq[ColumnInfo] = columnData.getOrElseUpdate(typ, createGenericColData(typ))

  def createGenericColData(typ: Int): IndexedSeq[ColumnInfo] = {
    val myClass = AllClasses.get.getClassByID(typ)
    for (ix <- myClass.fields.indices; if myClass.fieldSetting(ix).visible) yield new ColumnInfo(ix, 50)
  }

  def moveColumn(typ: Int, fromIx: Int, toIx: Int): Unit = if (fromIx != toIx) {
    val oldList: Seq[ColumnInfo] = columnData.get(typ) match {
      case Some(list) if fromIx < list.size && toIx < list.size => list
      case _ => AllClasses.get.getClassByID(typ).fields.indices.
        map(new ColumnInfo(_, 60))
    }
    if(fromIx<oldList.size){
    val from = oldList(fromIx)
    val buffer = new ArrayBuffer[ColumnInfo] ++= oldList
    buffer.remove(fromIx)
    buffer.insert(/*(if(toIx>fromIx)-1 else 0)+*/ toIx, from)
    columnData(typ) = buffer
    WebSocketConnector.sendMessage("ChangeTableSetup|" + typ + "§" + buffer.mkString(";"))
    } else Log.e("MoveColumn fromIx:"+fromIx+" oldList.size:"+oldList.size)
  }
}
