abstract class DynamicClass extends SuperClass with Dynamic {
  def applyDynamic()(implicit m: Dynamic.MethodName)
}

class SuperClass {
  private def privateMethod()= ()
}

abstract class Test {
  val test: DynamicClass;
  test.privateMethod()
}
