package clientbase.connection

import definition.typ.AllClasses
import util._

import scala.collection.mutable.ArrayBuffer

/**
 * Created by Kathi on 08.07.2015.
 */

class ColumnInfo(val ix:Int,val width:Int){
  override def toString=ix.toString+","+width
}

object TableSettings{
  protected val columnData=collection.mutable.HashMap[Int,Seq[ColumnInfo]]()

  def load(settingsString:String) = {
    settingsString match {
      case BarSplitList(blist)=> for (b<-blist) b match {
        case ColonSplit(StrToInt(typ),setting)=> setting match {
          case SemicolonSplitList(secList)=>
            val myClass=AllClasses.get.getClassByID(typ)
            val colBuffer= ArrayBuffer[ColumnInfo]()
            for(s<-secList) s match {
              case CommaSplit(StrToInt(ix),StrToInt(width))=> if(myClass.fieldSetting(ix).visible) colBuffer+=new ColumnInfo(ix,width)
              case other => Log.e("unknown columnSetting for type "+typ+" :"+other+" in "+setting)
            }
            columnData(typ)=colBuffer
          case other => Log.e("unknown tableSetting for type "+typ+" :"+other)
        }
        case other => Log.e("unknown tableSetting "+other)
      }
      case other => Log.e("unknown tableSettings "+other)
    }
  }

  def createGenericColData(typ:Int)= {
    val myClass=AllClasses.get.getClassByID(typ)
    for(ix<-0 until myClass.fields.size;if(myClass.fieldSetting(ix).visible))yield new ColumnInfo(ix,50)
  }

  def getColumnData(typ:Int):Seq[ColumnInfo]=columnData.getOrElseUpdate(typ,createGenericColData(typ))

  def moveColumn(typ:Int,fromIx:Int,toIx:Int)= if(fromIx!=toIx){
    val oldList=  columnData.get(typ) match {
      case Some(list) if(fromIx<list.size&&toIx<list.size)=>list
      case _=>(0 until AllClasses.get.getClassByID(typ).fields.size).
        map(new ColumnInfo(_,60))
    }
    val from=oldList(fromIx)
    val buffer=new ArrayBuffer[ColumnInfo]++=oldList
    buffer.remove(fromIx)
    buffer.insert(/*(if(toIx>fromIx)-1 else 0)+*/toIx,from)
    columnData(typ)=buffer
    WebSocketConnector.sendMessage("ChangeTableSetup|"+typ+"§"+buffer.mkString(";"))
  }
}
