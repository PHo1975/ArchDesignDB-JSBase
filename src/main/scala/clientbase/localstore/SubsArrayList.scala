package clientbase.localstore

import clientbase.connection.{Subscriber, WebSocketConnector}
import definition.data.{InstanceData, Referencable, Reference}
import util.Log

import scala.collection.mutable.ArrayBuffer

/**
  * Created by Peter on 07.02.2016.
  */
trait SubsArrayList[A <:Referencable] extends Subscriber[A] {
  val list: ArrayBuffer[A] =ArrayBuffer[A]()
  var doneCallBack:()=>Unit=_

  def load(parentRef:Reference,propField:Int,doneListener:()=>Unit):Unit={
    list.clear
    doneCallBack=doneListener
    if(subsID== -1) WebSocketConnector.createSubscription(parentRef,propField,this)
    else Log.e("Load parentRef:"+parentRef+" propfield:"+propField+" subsID != -1 ")
  }

  override def onLoad(data: Iterator[A]): Unit ={
    list.clear()
    onUpdate(data)
    doneCallBack()
  }

  override def onUpdate(data: Iterator[A]): Unit ={
    for(l<-list)destructor(l)
    list.clear()
    for(d<-data)list++=data
    update(list)
  }

  override def onChange(data: A): Unit = {
    val ix= list.indexWhere(_.ref==data.ref)
    if(ix> -1){
      destructor(list(ix))
      list(ix)=data
      updateElement(data)
    } else Log.e("on change "+data+": not found")
  }

  override def onDelete(data: Reference): Unit = {
    val ix= list.indexWhere(_.ref==data.ref)
    if(ix> -1) {
      destructor(list(ix))
      list.remove(ix)
    } else Log.e("on delete "+data+": not found")
  }

  override def onChildAdded(data: A): Unit ={
    list +=data
    updateElement(data)
  }

  def factory(data:InstanceData):A

  def update(list:IndexedSeq[A]):Unit = {}

  def updateElement(el:A):Unit= {}

  def destructor(elem:A):Unit = {}

  override def unsubscribe():Unit={
    super.unsubscribe()
    list.clear()
  }
}

