package clientbase.localstore

import clientbase.connection.{InstSubscriber, WebSocketConnector}
import definition.data.{InstanceData, Referencable, Reference}
import util.Log

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Peter on 07.02.2016.
  */
trait SubsArrayList[A <: Referencable] extends InstSubscriber {
  val list: ArrayBuffer[A] = ArrayBuffer[A]()
  var doneCallBack: () => Unit = _

  def load(parentRef: Reference, propField: Int, doneListener: () => Unit): Unit = {
    list.clear()
    doneCallBack = doneListener
    if (subsID == -1) WebSocketConnector.createSubscription(parentRef, propField, this)
    else Log.e("Load parentRef:" + parentRef + " propfield:" + propField + " subsID != -1 ")
  }

  override def onLoad(data: Iterator[InstanceData]): Unit = {
    list.clear()
    onUpdate(data)
    doneCallBack()
  }

  override def onUpdate(data: Iterator[InstanceData]): Unit = {
    for (l <- list) destructor(l)
    list.clear()
    for (d <- data) list += dataFactory(d)
    update(list)
  }

  def update(list: Iterable[A]): Unit = {}

  override def onChange(data: InstanceData): Unit = {
    val ix = list.indexWhere(_.ref == data.ref)
    if (ix > -1) {
      destructor(list(ix))
      val elData = dataFactory(data)
      list(ix) = elData
      updateElement(elData)
    } else Log.e("on change " + data + ": not found")
  }

  override def onDelete(data: Reference): Unit = {
    val ix = list.indexWhere(_.ref == data.ref)
    if (ix > -1) {
      destructor(list(ix))
      list.remove(ix)
    } else Log.e("on delete " + data + ": not found")
  }

  def destructor(elem: A): Unit = {}

  override def onChildAdded(data: InstanceData): Unit = {
    val elData = dataFactory(data)
    list += elData
    updateElement(elData)
  }

  def updateElement(el: A): Unit = {}

  def dataFactory(data: InstanceData): A

  override def unsubscribe(): Unit = {
    super.unsubscribe()
    list.clear()
  }
}

