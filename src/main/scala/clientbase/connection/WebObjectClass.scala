package clientbase.connection

import java.io.DataInput

import definition.typ._
import definition.typ.form.AbstractFormBox

import scala.collection.mutable


/**
  * Created by Peter    on 24.05.2015.
  */
class WebObjectClass(val name: String, val description: String, val id: Int, val ownFields: Seq[AbstractFieldDefinition], val ownFieldSettings: Seq[FieldSetting],
                     val ownPropFields: Seq[PropertyFieldDefinition],  val ownBlockPropFields: Seq[BlockPropertyFieldDefinition], val ownActions: Iterable[ActionTrait], val superClasses: Array[Int], val shortFormat: InstFormat,
                     val resultFormat: InstFormat) extends AbstractObjectClass {
  var enumFields: Map[Int, EnumDefinition] = Map.empty

  def comment = ""

  override def resolveSuperFields(): Unit = if (!hasResolved) {
    super.resolveSuperFields()
    enumFields = fields.view.zipWithIndex.collect(
      { case (enumField: EnumFieldDefinition, ix) => (ix, SystemSettings.settings.enumByID(enumField.enumID)) }).toMap
  }

  override def longFormat: InstFormat = shortFormat

  override def formBox: Option[AbstractFormBox] = None

  override def customInstanceEditor: Option[String] = None

  override def importDescriptor: Option[String] = None
}


object WebObjectClass {
  def fromStream(in: DataInput): WebObjectClass = {
    new WebObjectClass(in.readUTF(), in.readUTF(), in.readInt(),
      for (_ <- 0 until in.readInt) yield FieldDefinition.fromStream(in),
      for (_ <- 0 until in.readInt) yield FieldSetting.fromStream(in),
      for (_ <- 0 until in.readInt) yield PropertyFieldDefinition.fromStream(in),
      for (_ <- 0 until in.readInt) yield BlockPropertyFieldDefinition.fromStream(in),
      for (_ <- 0 until in.readInt()) yield ActionDescription.fromStream(in),
      (for (_ <- 0 until in.readInt()) yield in.readInt()).toArray,
      InstFormat.fromString(in.readUTF()),
      InstFormat.fromString(in.readUTF())
    )
  }
}


class WebClasses extends AllClasses[WebObjectClass] {
  val classList: mutable.Map[Int, WebObjectClass] = collection.mutable.Map[Int, WebObjectClass]()
  val blockClassList:mutable.Map[Int,BlockClass]=collection.mutable.Map[Int,BlockClass]()

  def readClasses(in: DataInput): Unit = {
    val numClasses = in.readInt()
    //println("Num classes:"+numClasses)
    for (i <- 0 until numClasses) {
      val cl = WebObjectClass.fromStream(in)
      //println("class "+cl.name+" read done")
      classList(cl.id) = cl
    }
    val numBlockClasses=in.readInt()
    for (i<- 0 until numBlockClasses){
      val bl=BlockClass.fromStream(in)
      blockClassList(bl.id)=bl
    }
    //println("read classes "+numClasses)
    AllClasses.set(this)
  }

}
