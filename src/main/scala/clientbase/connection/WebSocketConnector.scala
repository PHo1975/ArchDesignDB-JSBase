package clientbase.connection

import java.io.{DataInput, DataInputStream}
import definition.comm._
import definition.data._
import definition.expression.{CommonFuncMan, Constant, Expression, FunctionManager}
import definition.typ.SystemSettings
import org.scalajs.dom._
import util.Log

import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.scalajs.js.typedarray.{ArrayBuffer}
import scala.util.control.NonFatal

/**
  * Created by Peter   on 05.04.2015.
  */

object UserSettings extends UserSetting


object WebSocketConnector {
  type LoadCallback= Seq[InstanceData] =>Unit
  val host: String = window.location.host
  protected val newSubscriberQueue: mutable.Queue[Subscriber[_]] = collection.mutable.Queue[Subscriber[_]]()
  //val newBlockSubscriberQueue:mutable.Queue[Subscriber[BlockData]]= collection.mutable.Queue[Subscriber[BlockData]]()
  protected val subscriberMap: mutable.HashMap[Int, Subscriber[_]] = collection.mutable.HashMap[Int, Subscriber[_]]()
  protected val blockSubscriberMap: mutable.HashMap[Int, Subscriber[BlockData]] = collection.mutable.HashMap[Int, Subscriber[BlockData]]()
  protected val loadCallbackMap: mutable.HashMap[Int,LoadCallback] = collection.mutable.HashMap[Int, LoadCallback]()
  //val loadDataQueue:mutable.Queue[LoadCallback]=mutable.Queue[LoadCallback]()
  var webSocket: Option[WebSocket] = None
  protected var readyCallBack: Option[() => Unit] = None
  var root: Reference = EMPTY_REFERENCE
  protected var _userID: Int = _
  def userID: Int =_userID
  protected var _editable = false
  def editable: Boolean =_editable
  protected var loadTicket=0
  //var commandResultCallBack: Option[Constant => Unit] = None
  protected var commandResultPromise:Option[Promise[Constant]]= None
  protected var calendarDataCallBack: Option[DataInput => Unit] = None
  protected var typesLoaded = false

  FunctionManager.setManager(new CommonFuncMan)

  protected def withWebSocket(callback: WebSocket => Unit): Unit = webSocket match {
    case Some(ws) => if (ws.readyState == 1) callback(ws)
                     else {
                       ws.close()
                       ws.onopen = null
                       ws.onmessage = null
                       setupWebSocket(callback)
                     }
    case _ => setupWebSocket(callback)
  }

  def sendMessage(message: String): Unit = withWebSocket {
    _.send(message)
  }

  def sendBinary(buffer:ArrayBuffer):Unit= withWebSocket {
    _.send(buffer)
  }

  def createSubscription[A <: Referencable](ref: Reference, field: Int, subscriber: Subscriber[A]): Unit = if (!typesLoaded) notifyTypesNotLoaded() else {
    //println("Create Subs ref:"+ref+" field: "+field)
    newSubscriberQueue += subscriber
    sendMessage("CreateSubscription|" + ref.bToString + "," + field)
  }

  def createPathSubscription[A <: Referencable](ref: Reference, subscriber: Subscriber[A]): Unit = if (!typesLoaded) notifyTypesNotLoaded() else {
    newSubscriberQueue += subscriber
    sendMessage("SubscribePath|" + ref.bToString)
  }

  def createBlockSubscription[A <: Referencable](parentRef:Reference,field:Int,subscriber:Subscriber[A]):Unit = if(!typesLoaded) notifyTypesNotLoaded() else {
    newSubscriberQueue +=subscriber
    sendMessage("SubscribeBlocks|"+parentRef.bToString+","+field)
  }

  def loadChildren(ref:Reference,propField:Int,callBack: Seq[InstanceData] =>Unit):Unit= {
    loadTicket+=1
    loadCallbackMap(loadTicket)=callBack
    sendMessage("LoadData|"+ref.bToString+"|"+propField.toString+"|"+loadTicket)
  }

  def pathOpenChild(subsID: Int, ref: Reference): Unit =
    if (!typesLoaded) notifyTypesNotLoaded() else sendMessage("OpenChild|" + subsID + "|" + ref.bToString)

  def pathJumpUp(subsID: Int, pos: Int): Unit =
    if (!typesLoaded) notifyTypesNotLoaded() else sendMessage("JumpUp|" + subsID + "|" + pos)

  def removeSubscription(subsID: Int): Unit = {
    sendMessage("RemoveSubscription|" + subsID)
    if (subscriberMap.contains(subsID))
      subscriberMap.remove(subsID)
    else Log.e("remove Subs " + subsID + " not found")
  }

  def start(appName: String, callback: () => Unit): Unit = {
    readyCallBack = Some(callback)
    sendMessage("Root|" + appName)
  }

  def writeInstanceField(ref: Reference, field: Int, value: Expression): Unit =
    sendMessage("WriteField|" + ref.sToString + "|" + field + "|" + value.encode)

  def writeInstancesField(refs: Iterable[Referencable],field:Int,value:Expression):Unit=
    sendMessage("WriteInstancesField|"+refs.map(_.ref.bToString).mkString("#")+"|"+field+"|"+value.encode)

  protected def createCommandResultPromise(): Future[Constant] ={
    if(commandResultPromise.isDefined) throw new IllegalArgumentException("CommandPromise already defined")
    else {
      val p=Promise[Constant]()
      commandResultPromise=Some(p)
      p.future
    }
  }

  def executeAction(owner: OwnerReference, instList: Iterable[Referencable], actionName: String, params: Iterable[ResultElement]): Future[Constant] = {
    val ret=createCommandResultPromise()
    sendMessage("Execute|"+owner.sToString+"|"+ instList.map(_.ref.bToString).mkString(";") + "|" + actionName + "|" +
      (if(params.isEmpty) " " else params.map(e => e.paramName + "\u2192" + e.result.encode).mkString("\u01c1")))
    ret
  }

  def executeCreateAction(owner:Reference,propField:Byte,createType:Int,actionName:String,params:Iterable[ResultElement],formatValues:Iterable[(Int,Constant)]): Future[Constant] ={
    val ret=createCommandResultPromise()
    sendMessage("ExecuteCreate|"+owner.bToString+"|"+propField.toString+"|"+createType.toString+"|"+actionName+"|"+
      params.map(e => e.paramName + "\u2192" + e.result.encode).mkString("\u01c1")+"|"+
      (if(formatValues.isEmpty)"*" else formatValues.map(f=> f._1.toString+"\u2192"+f._2.encode).mkString("\u01c1")))
    ret
  }

  def createInstance(typ: Int, owners: Array[OwnerReference]): Future[Constant] = {
    val ret=createCommandResultPromise()
    sendMessage("CreateInstance|" + typ + "|" + owners.map(_.sToString).mkString(";"))
    ret
  }

  def createBlock(typ:Int,owner:OwnerReference,newData:Array[Byte]): Future[Constant] = {
    val ret=createCommandResultPromise()
    sendBinary(TransferBuffersMap.createArrayBuffer(typ,-1,owner,newData))
    ret
  }

  def changeBlock(owner:OwnerReference,newData:BlockData): Unit = {
    sendBinary(TransferBuffersMap.createArrayBuffer(newData.ref.typ,newData.ref.instance,owner,newData.data))
  }

  def deleteBlock(ref:Reference,owner:OwnerReference): Unit ={
    sendMessage("DeleteBlock|"+ref.sToString+"|"+owner.sToString)
  }

  def deleteInstance(ref: Reference): Unit = {
    commandResultPromise = None
    sendMessage("DeleteInstance|" + ref.sToString)
  }


  // ***************************************************
  // Server command handlers
  // ***************************************************



  protected def acceptSubscription(in: DataInput): Unit = {
    val subsID = in.readInt()
    println("accept Subs ID "+subsID)
    if (newSubscriberQueue.isEmpty) Log.e("acceptSubs but queue is empty")
    else {
      val newSubscriber: Subscriber[Referencable] = newSubscriberQueue.dequeue().asInstanceOf[Subscriber[Referencable]]
      if (subsID == -1) newSubscriber.onFailed(in.readUTF())
      else {
        newSubscriber.subsID = subsID
        subscriberMap(subsID) = newSubscriber
        val dataList = for (_ <- Iterator.range(0, in.readInt())) yield newSubscriber.factory(in)
        newSubscriber.onLoad(dataList)
      }
    }
  }

  protected def loadTypes(in: DataInput): Unit = {
    try {
      val classList = new WebClasses
      classList.readClasses(in)
      TableSettings.load(in.readUTF())
    } catch {
      case NonFatal(e) => Log.e("load Types", e)
    }
    typesLoaded = true
    for (c <- readyCallBack) c()
  }

  protected def loadSystemSettings(in: DataInput): Unit = {
    SystemSettings.settings = new WebSystemSettings(in)
    sendMessage("SendTypes")
  }

  protected def subscriptionNotification(in: DataInput): Unit = {
    val subsID = in.readInt()
    val subscriber: Subscriber[Referencable] = if(subscriberMap.contains(subsID)) subscriberMap(subsID).asInstanceOf[Subscriber[Referencable]] else {
        Log.e("subscriptionNotification unknown SubsID "+subsID)
        null
      }
    val nt = NotificationType(in.readInt())
    if(subscriber!=null) nt match {
      case NotificationType.fieldChanged =>
        subscriber.onChange(subscriber.factory(in))
      case NotificationType.childAdded =>
        subscriber.onChildAdded(subscriber.factory(in))
      case NotificationType.instanceRemoved =>
        subscriber.onDelete(Reference(in))
      case NotificationType.sendData => subscriber.onLoad(readList(in, subscriber.factory))
      case NotificationType.updateUndo => subscriber.onUpdate(readList(in, subscriber.factory))
      case o=> Log.e("unknown Notification "+o)
    }
  }

  protected def sendUserSettings(in: DataInput): Unit = {
    SystemSettings.settings = new WebSystemSettings(in)
  }



  protected def serverCommandResponse(in: DataInput): Unit = {
    println("Server Command Response, callback "+commandResultPromise.isDefined)
    val hasError = in.readBoolean
    if (hasError) {
      val error = CommandError.read(in)
      Log.e(error.getMessage)
      for(p<-commandResultPromise)
        p.failure(error)
    }
    else if (in.readBoolean) {
      val data = Expression.readConstant(in)
      commandResultPromise match {
        case Some(p) =>
          commandResultPromise=None
          try {
            p.success(data)
          } catch {
            case NonFatal(e)=>Log.e("Sending success",e)
          }
        case None => Log.e("response without callback, data: " + data)
      }
    }
    commandResultPromise=None
  }

  def registerCalendarReceiver(callBack: DataInput => Unit): Unit = {
    calendarDataCallBack = Some(callBack)
    sendMessage("CalendarRoots")
  }

  protected def receiveCalendarData(in: DataInput): Unit =
    for (c <- calendarDataCallBack) {
      c(in)
      calendarDataCallBack = None
    }


  protected def receiveQueryResponse(input: DataInput):Unit = {
    val dataTicket=input.readInt()
    val dataList= for (_ <- 0 until input.readInt()) yield Subscriber.readInstance(input)
    if(loadCallbackMap.contains(dataTicket)) {
      loadCallbackMap(dataTicket)(dataList)
      loadCallbackMap.remove(dataTicket)
    }
    else Log.e("receiveQueryResponse unknown Ticket :"+dataTicket+" size:"+dataList.size)
  }

  private def setupWebSocket(callback: WebSocket => Unit): Unit = {
    println("host="+host)
    println("prot:"+window.location.port)
    val ws = new WebSocket((if (host.startsWith("localhost")||window.location.port=="1080") "ws://" else "wss://") + host + "/events" )
    webSocket = Option(ws)
    if (ws != null) {
      ws.onopen = (_: Event) => callback(ws)
      ws.onmessage = onMessage _
      ws.onerror = onError _
    } else println("websocket==null " + host)
  }

  private def onMessage(ev: MessageEvent): Unit =
    ev.data match {
      case b: Blob =>
        val fr = new FileReader
        fr.onloadend = (_: ProgressEvent) => {
          fr.result match {
            case a: ArrayBuffer =>
              val nbuffer = new ArrayBufferInputStream(a)
              val inStream = new DataInputStream(nbuffer)
              while (inStream.available() > 0)
                handleServerCommands(inStream.readInt(), inStream)
            case o => Log.e("unknown " + o)
          }
        }
        fr.onerror = (e: Event) => Log.e("error " + e)
        fr.readAsArrayBuffer(b)
      case o => Log.e("o:" + o.toString)
    }


  private def handleServerCommands(command: Int, data: DataInput): Unit = {
    val com = ServerCommands(command)
    //println("server command "+com)
    com match {
      case ServerCommands.sendUserSettings =>
        _editable = data.readBoolean()
        _userID = data.readInt()
        root = Reference(data.readInt(), data.readInt())
        UserSettings.readFromStream(data, data.readInt(), atClient = true)
        val numKeyStrokeGroups = data.readInt()
        for (_ <- 0 until numKeyStrokeGroups) {
          data.readUTF()
          val numCommands = data.readInt()
          for (_ <- 0 until numCommands) {
            data.readUTF()
            data.readInt()
            data.readInt()
          }
        }
        sendMessage("SendSystemSettings")

      case ServerCommands.acceptSubscription => acceptSubscription(data)
      case ServerCommands.sendTypes => loadTypes(data)
      case ServerCommands.sendSystemSettings => loadSystemSettings(data)
      case ServerCommands.sendSubscriptionNotification => subscriptionNotification(data)
      case ServerCommands.sendCommandResponse => serverCommandResponse(data)
      case ServerCommands.sendCalendarData => receiveCalendarData(data)
      case ServerCommands.sendQueryResponse=> receiveQueryResponse(data)
      case o => Log.e("wrong Server command " + o.toString)
    }
  }

  private def onError(e: Event): Unit =
     Log.e(e.toString)




  private def notifyTypesNotLoaded(): Unit = {
    println("Types not loaded yet")
    println(Thread.currentThread().getStackTrace.mkString("\n   "))
  }

  private def readList[A](in: DataInput, factory: DataInput => A): Iterator[A] =
    for (_ <- Iterator.range(0, in.readInt())) yield factory(in)
}