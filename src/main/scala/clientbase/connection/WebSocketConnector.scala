package clientbase.connection

import java.io.{DataInput, DataInputStream}

import definition.comm._
import definition.data._
import definition.expression.{CommonFuncMan, Constant, Expression, FunctionManager}
import definition.typ.SystemSettings
import org.scalajs.dom.raw._
import org.scalajs.dom.window
import util.Log

import scala.collection.mutable
import scala.scalajs.js.typedarray.{ArrayBuffer, ArrayBufferInputStream}

/**
 * Created by Peter on 05.04.2015.
 */

object UserSettings extends UserSetting

object WebSocketConnector {
  val host: String =window.location.host
  var webSocket:Option[WebSocket]=None
  var readyCallBack:Option[ () => Unit]=None
  var root:Reference=EMPTY_REFERENCE
  var userID:Int=_
  var editable=false
  var newSubscriberQueue: mutable.Queue[Subscriber[_]] = collection.mutable.Queue[Subscriber[_]]()
  var subscriberMap: mutable.HashMap[Int, Subscriber[_]] =collection.mutable.HashMap[Int,Subscriber[_]]()
  var commandResultCallBack:Option[(Constant)=>Unit]=None
  var calendarDataCallBack:Option[(DataInput)=>Unit]=None

  FunctionManager.setManager(new CommonFuncMan)

  private def setupWebSocket(callback:WebSocket=>Unit):Unit= {
    val ws=new WebSocket("ws://"+host.toString +"/events")
    webSocket=Some(ws)
    ws.onopen= ( ev:Event)=> callback(ws)
    ws.onmessage=onMessage _
    ws.onerror=onError _
  }

  private def onMessage(ev:MessageEvent):Unit={
    ev.data match {
      case b:Blob=>
        val fr=new FileReader
        fr.onloadend=(e:ProgressEvent)=>{
          fr.result match {
            case a:ArrayBuffer =>
              val nbuffer= new ArrayBufferInputStream(a)
              val inStream=new DataInputStream(nbuffer)
              while(nbuffer.available()>0)
                handleServerCommands(inStream.readInt(),inStream)
            case o=> Log.e("unknown "+o)
          }
        }
        fr.onerror=(e:Event)=> Log.e("error "+e)
        fr.readAsArrayBuffer(b)
      case o => Log.e("o:"+o.toString)
    }
  }

  private def handleServerCommands(command:Int,data:DataInput):Unit= {
    val com=ServerCommands(command)
    //println("command "+com)
    com match {
      case ServerCommands.sendUserSettings=>
        editable=data.readBoolean()
        userID=data.readInt()
        root=Reference(data.readInt(),data.readInt())
        UserSettings.readFromStream(data,data.readInt(),atClient = true)
        sendMessage("SendSystemSettings")

      case ServerCommands.acceptSubscription=> acceptSubscription(data)
      case ServerCommands.sendTypes=> loadTypes(data)
      case ServerCommands.sendSystemSettings=> loadSystemSettings(data)
      case ServerCommands.sendSubscriptionNotification=> subscriptionNotification(data)
      case ServerCommands.sendCommandResponse=> serverCommandResponse(data)
      case ServerCommands.sendCalendarData=> receiveCalendarData(data)
      case o=> Log.e("wrong Server command "+o.toString)
    }
  }

  private def onError(ev:ErrorEvent):Unit ={
    Log.e(ev.message+" "+ev.filename+" line:"+ev.lineno)
  }

  def withWebSocket(callback:WebSocket=>Unit):Unit= webSocket match{
    case Some(ws) => if(ws.readyState==1) callback(ws)
      else {
        ws.close()
        ws.onopen=null
        ws.onmessage=null
        setupWebSocket(callback)
      }
    case _=> setupWebSocket(callback)
  }

  def sendMessage(message:String):Unit= {
    //println("send Message ["+message+"]")
    withWebSocket{ _.send(message) }
  }

  def createSubscription[A <: Referencable](ref:Reference, field:Int, subscriber:Subscriber[A]):Unit= {
    //println("Create Subs ref:"+ref+" field: "+field)
    newSubscriberQueue+=subscriber
    sendMessage("CreateSubscription|"+ref.bToString()+","+field)
  }

  def createPathSubscription[A <: Referencable](ref:Reference,subscriber:Subscriber[A]):Unit = {
    //println("Create Path Subs "+ref)
    newSubscriberQueue+=subscriber
    sendMessage("SubscribePath|"+ref.bToString())
  }

  def pathOpenChild(subsID:Int,ref:Reference):Unit={
    sendMessage("OpenChild|"+subsID+"|"+ref.bToString())
  }

  def pathJumpUp(subsID:Int,pos:Int):Unit={
    sendMessage("JumpUp|"+subsID+"|"+pos)
  }

  def removeSubscription(subsID:Int):Unit={
    sendMessage("RemoveSubscription|"+subsID)
    if(subscriberMap.contains(subsID))
      subscriberMap.remove(subsID)
    else Log.e("remove Subs "+subsID+" not found")
  }

  def start(appName:String,callback: ()=>Unit):Unit= {
    readyCallBack=Some(callback )
    sendMessage("Root|"+appName)
  }

  def writeInstanceField(ref:Reference,field:Int,value:Expression):Unit= {
    sendMessage("WriteField|"+ref.sToString+"|"+field+"|"+value.encode)
  }
  def executeAction(owner:OwnerReference,instList:Iterable[Referencable],actionName:String,params:Seq[(String,Constant)]):Unit={
    commandResultCallBack=None
    sendMessage("Execute|"+instList.map(_.ref.bToString()).mkString(";")+"|"+actionName+"|"+params.map(e=>e._1+"\u2192"+e._2.encode).mkString("\u01c1"))
  }

  def createInstance(typ:Int,owners:Array[OwnerReference],callBack:(Constant)=>Unit):Unit= {
    commandResultCallBack=Some(callBack)
    sendMessage("CreateInstance|" + typ + "|" + owners.map(_.sToString).mkString(";"))
  }

  def deleteInstance(ref:Reference):Unit = {
    commandResultCallBack=None
    sendMessage("DeleteInstance|"+ref.sToString)
  }



  // ***************************************************
  // Server command handlers

  def acceptSubscription(in:DataInput):Unit= {
    val subsID=in.readInt()
    //println("accept Subs ID "+subsID)
    if(newSubscriberQueue.isEmpty) Log.e("acceptSubs but queue is empty")
    else {
      val newSubscriber: Subscriber[Referencable] = newSubscriberQueue.dequeue().asInstanceOf[Subscriber[Referencable]]
      if(subsID== -1) newSubscriber.onFailed(in.readUTF())
      else {
        newSubscriber.subsID = subsID
        subscriberMap(subsID) = newSubscriber
        val dataList = for (_ <- Iterator.range(0,in.readInt())) yield newSubscriber.factory(in)
        newSubscriber.onLoad(dataList)
      }
    }
  }


  def loadTypes(in:DataInput):Unit = {
    val classList=new WebClasses
    classList.readClasses(in)
    TableSettings.load(in.readUTF())
    for(c<-readyCallBack) c()
  }

  def loadSystemSettings(in:DataInput):Unit={
    SystemSettings.settings=new WebSystemSettings(in)
    //println("enums: "+SystemSettings().enums.mkString("|"))
    sendMessage("SendTypes")
  }

  private def readList[A](in:DataInput,factory:DataInput=>A):Iterator[A] =
    for(_ <- Iterator.range(0,in.readInt())) yield factory(in)



  def subscriptionNotification(in:DataInput):Unit= {
    val subsID=in.readInt()
    val subscriber: Subscriber[Referencable] =subscriberMap(subsID).asInstanceOf[Subscriber[Referencable]]
    val nt=NotificationType(in.readInt())
    Log.e("subs notify subsID:"+subsID+" "+nt)
    nt match {
      case NotificationType.fieldChanged =>
        subscriber.onChange(subscriber.factory(in))
      case NotificationType.childAdded =>
        subscriber.onChildAdded(subscriber.factory(in))
      case NotificationType.instanceRemoved =>
        subscriber.onDelete(Reference(in))
      case NotificationType.sendData => subscriber.onLoad(readList(in,subscriber.factory))
      case NotificationType.updateUndo => subscriber.onUpdate(readList(in,subscriber.factory))
    }
  }

  def sendUserSettings(in:DataInput):Unit= {
    SystemSettings.settings=new WebSystemSettings(in)
  }

  def serverCommandResponse(in:DataInput):Unit={
    val hasError=in.readBoolean
    if(hasError) {
      val error=CommandError.read(in)
      Log.e( error.getMessage)
    }
    else if (in.readBoolean) {
        val data=Expression.readConstant(in)
        commandResultCallBack match {
          case Some(call)=> call(data)
          case None => Log.e("response without callback, data: "+data)
        }
      }

  }

  def registerCalendarReceiver(callBack:DataInput=>Unit):Unit={
    calendarDataCallBack=Some(callBack)
    sendMessage("CalendarRoots")
  }

  def receiveCalendarData(in:DataInput):Unit=
    for(c<- calendarDataCallBack) {
      c(in)
      calendarDataCallBack=None
    }
}