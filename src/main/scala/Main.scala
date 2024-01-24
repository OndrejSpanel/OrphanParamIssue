import Macros.*

object Types {
  trait Holder[M[_]]

  class MyTask[TaskA]

  object Holder {
    type BySkinny[SkinnyA] = MyTask[SkinnyA]
    def bySkinny: Holder[BySkinny] = ???
  }
}

import Types._

@main
def main: Unit = {
  val doc = typeOf[Holder[Holder.BySkinny]]
  println(doc)
}
