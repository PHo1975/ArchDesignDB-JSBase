package clientbase.connection

import java.io.DataInput

import definition.data.{InstanceData, Referencable, Reference}
import org.scalajs.dom

/**
  * Created by Peter  on 25.05.2015.
  */
trait Subscriber[A <: Referencable] {
  var subsID: Int = -1

  def factory(in: DataInput): A

  def onLoad(data: Iterator[A]): Unit

  def onUpdate(data: Iterator[A]): Unit

  def onChange(data: A): Unit

  def onDelete(data: Reference): Unit

  def onChildAdded(data: A): Unit

  def onFailed(reason: String): Unit = dom.window.alert(reason)

  def unsubscribe(): Unit = {
    if (subsID > -1) WebSocketConnector.removeSubscription(subsID)
    subsID = -1
  }
}


trait InstSubscriber extends Subscriber[InstanceData] {
  def factory(in: DataInput): InstanceData = Subscriber.readInstance(in)
}


object Subscriber {
  val doNothing: () => Unit = () => {}

  def readInstance(in: DataInput): InstanceData = InstanceData.readWithChildInfo(Reference(in), in)
}
