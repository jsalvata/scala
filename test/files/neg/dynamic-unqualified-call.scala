class DynamicClass extends Dynamic {
  def applyDynamic()(implicit m: Dynamic.MethodName) = ()
  nonExistingMethod()
}
