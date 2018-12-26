package clientbase.localstore

import clientbase.connection.{InstSubscriber, Subscriber, WebSocketConnector}
import definition.data.{InstanceData, Referencable, Reference}
import util.Log

/**
  * Created by Peter Holzer on 27.02.2017 .
  */
trait SingleObjectSubscriber[A <: Referencable] extends Subscriber[A] {
  var value: A = _

  var doneCallBack: Option[() => Unit] = None

  def dataChanged: A => Unit

  def onLoad(data: Iterator[A]): Unit = {
    onUpdate(data)
    for (l <- doneCallBack) l()
  }

  def onUpdate(data: Iterator[A]): Unit = if (data.hasNext) onChange(data.next)

  def onChange(data: A): Unit = {
    value = data
    dataChanged(value)
  }

  def onDelete(data: Reference): Unit = {}

  def onChildAdded(data: A): Unit = {}
}


class SingleInstanceSubs(val dataChanged: InstanceData => Unit) extends
  SingleObjectSubscriber[InstanceData] with InstSubscriber {

  def load(parentRef: Reference, propField: Int, doneListener: Option[() => Unit] = None): Unit = {
    doneCallBack = doneListener
    if (subsID == -1) WebSocketConnector.createSubscription(parentRef, propField, this)
    else Log.e("Load single parentRef:" + parentRef + " propfield:" + propField + " subsID != -1 ")
  }
}