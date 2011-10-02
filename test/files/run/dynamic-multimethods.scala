object Test1 extends Dynamic {
  def applyDynamic(b: Boolean)(implicit m: Dynamic.MethodName) = "Boolean: "+m+"("+b+")"
  def applyDynamic(i: Int)(implicit m: Dynamic.MethodName) = "Int: "+m+"("+i+")"
  def applyDynamic()(implicit m: Dynamic.MethodName) = "Nothing: "+m+"()"

  def run() {
    println(this.anInt(1))
    println(this.aBoolean(false))
    println(this.aNothing())
    println(this.anotherNothing)
  }
}

object Test2 extends Dynamic {
  def applyDynamic(implicit m: Dynamic.MethodName) = "Really nothing: "+m

  def run() {
    println(this.reallyNothing)
  }
}

object Test3 extends Dynamic {
  def applyDynamic(b: Boolean) = "Boolean: "+b
  def applyDynamic(i: Int) = "Int: "+i
  def applyDynamic() = "Nothing: ()"

  def run() {
    println(this.anInt(1))
    println(this.aBoolean(false))
    println(this.aNothing())
    println(this.anotherNothing)
  }
}

object Test4 extends Dynamic {
  def applyDynamic = "Really nothing"

  def run() {
    println(this.reallyNothing)
  }
}

object Test extends App {
  Test1.run();
  Test2.run();
  Test3.run();
  Test4.run();
}
