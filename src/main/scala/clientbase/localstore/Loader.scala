package clientbase.localstore

import scala.annotation.tailrec

/**
  * Created by Kathi on 08.02.2016.
  */
trait Loader {
   def load(callback:()=>Unit)
}

object Loader {
  def cascadedLoad(list:Seq[Loader],finishCallBack:()=>Unit)= {
    val size=list.size
    var current=0

    def internCallBack() :Unit={
      current+=1
      if(current>=size) finishCallBack()
      else list(current).load(internCallBack _)
    }

    list(0).load(internCallBack _)
  }
}
