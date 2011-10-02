object dynamicObject extends Dynamic {
  def applyDynamic()(implicit m: Dynamic.MethodName) = ()
  this.foo()
}
class dynamicClass extends Dynamic {
  def applyDynamic()(implicit m: Dynamic.MethodName) = ()
  this.bar
  dynamicObject.bar()
}
abstract class dynamicAbstractClass extends Dynamic {
  def applyDynamic(args: Any*)(implicit m: Dynamic.MethodName): Int
  this.pili(1, new dynamicClass, "hello");
}
trait dynamicTrait extends Dynamic {
  def applyDynamic(args: Any*)(implicit m: Dynamic.MethodName) = 1
  def two = 2
  this.mili(1,2,3)
  two
}
object dynamicMixin extends dynamicAbstractClass with dynamicTrait {
  this.foo(None)
}
