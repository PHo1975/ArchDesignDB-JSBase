package clientbase.localstore

import clientbase.connection.{Subscriber, WebSocketConnector}
import definition.data.{Referencable, Reference}
import util.Log

import scala.collection.mutable

/**
  * Created by Peter on 08.02.2016.
  */
trait SubsMap[A <: Referencable] extends Subscriber[A] {
  val map: mutable.LinkedHashMap[Reference, A] = mutable.LinkedHashMap[Reference, A]()
  var doneCallBack: () => Unit = _

  //def factory(in:DataInput):A
  def update(it: Iterator[A]): Unit

  def load(parentRef: Reference, propField: Int, doneListener: () => Unit): Unit = {
    doneCallBack = doneListener
    if (subsID == -1) WebSocketConnector.createSubscription(parentRef, propField, this)
    else Log.e("Load parentRef:" + parentRef + " propfield:" + propField + " subsID != -1 ")
  }

  override def onLoad(data: Iterator[A]): Unit = {
    map.clear()
    onUpdate(data)
    notifyDoneCallBack()
  }

  def notifyDoneCallBack(): Unit = if (doneCallBack != null) {
    doneCallBack()
    doneCallBack = null
  }

  override def onUpdate(data: Iterator[A]): Unit = {
    map.clear()
    for (d <- data) map(d.ref) = d
    update(map.valuesIterator)
  }

  override def onChange(data: A): Unit = {
    //println("on Change " + data.ref)
    map(data.ref) = data
    update(map.valuesIterator)
  }

  override def onDelete(data: Reference): Unit =
    for (old <- map.get(data)) {
      destructor(old)
      map.remove(data)
      update(map.valuesIterator)
    }

  def destructor(it: A): Unit = {}

  override def onChildAdded(data: A): Unit = {
    map(data.ref) = data
    update(map.valuesIterator)
  }
}
