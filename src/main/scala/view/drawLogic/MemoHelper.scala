package view.drawLogic

import view.drawLogic.MemoHelper.memoize

object MemoHelper {
  def memoize[I, O](f: I => O): I => O = new collection.mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }
}

object singletonList {
  private val memoized: Any => Seq[Any] = memoize(x => {
    Seq(x)
  })

  def apply[A](a: A): Seq[A] = {
    memoized(a).asInstanceOf[Seq[A]]
  }
}
