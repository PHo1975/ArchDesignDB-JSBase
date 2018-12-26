package clientbase.connection

import java.io.DataInput

import definition.data.{InstanceData, Reference}
import definition.typ.ClientSystemSettings

/**
  * Created by Peter on 07.02.2016.
  */
class WebSystemSettings(in: DataInput) extends ClientSystemSettings(in) {
  override def loadChildren(ref: Reference): IndexedSeq[InstanceData] = IndexedSeq.empty
}
