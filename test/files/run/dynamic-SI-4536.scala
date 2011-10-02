object Test extends App with Dynamic {
  def applyDynamic(a: Any*)(implicit m: Dynamic.MethodName) = "DynamicClass."+m+"("+a+")"
  def applyDynamic(implicit m: Dynamic.MethodName) = "DynamicClass."+m+"()"
  def regularMethod() = "Regular method"
  println(this.bar)
  println(this.bar())
  println(Test.bar())
  println(this.regularMethod)
  println(this.regularMethod())
  println(Test.regularMethod())
}
