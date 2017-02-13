package clientbase.localstore

import java.io.DataInput

import clientbase.connection.{Subscriber, WebSocketConnector}
import definition.data.{InstanceData, Referencable, Reference}
import util.Log

import scala.collection.{Map, mutable}

/**
  * Created by Kathi on 08.02.2016.
  */
trait SubsMap[A <: Referencable]  extends Subscriber[A] {
  def factory(in:DataInput):A
  def update(it:Iterator[A]):Unit
  def destructor: A=>Unit =(_:A)=>{}
  val map=mutable.LinkedHashMap[Reference,A]()
  var doneCallBack: ()=>Unit=_

  def load(parentRef:Reference,propField:Int,doneListener:()=>Unit):Unit ={
    doneCallBack=doneListener
    if(subsID== -1) WebSocketConnector.createSubscription(parentRef,propField,this)
    else Log.e("Load parentRef:"+parentRef+" propfield:"+propField+" subsID != -1 ")
  }

  override def onLoad(data: Iterator[A]): Unit ={
    map.clear()
    onUpdate(data)
    if(doneCallBack!=null) {
      doneCallBack()
      doneCallBack=null
    }
  }

  override def onUpdate(data: Iterator[A]): Unit ={
    map.clear()
    for(d<-data) map(d.ref)=d
    update(map.valuesIterator)
  }

  override def onChange(data: A): Unit = {
    map(data.ref)=data
    update(map.valuesIterator)
  }

  override def onDelete(data: Reference): Unit =
    for(old <-map.get(data)) {
      destructor(old)
      map.remove(data)
      update(map.valuesIterator)
    }


  override def onChildAdded(data: A): Unit ={
    map(data.ref)=data
    update(map.valuesIterator)

  }
}
