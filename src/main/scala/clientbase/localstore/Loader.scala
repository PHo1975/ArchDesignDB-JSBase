package clientbase.localstore

/**
  * Created by Peter on 08.02.2016.
  */
trait Loader {
  def load(callback: () => Unit)
}


object Loader {
  def cascadedLoad(list: Seq[Loader], finishCallBack: () => Unit): Unit = {
    val size = list.size
    var current = 0

    def internCallBack(): Unit = {
      current += 1
      if (current >= size) finishCallBack()
      else list(current).load(internCallBack _)
    }

    list.head.load(internCallBack _)
  }
}
