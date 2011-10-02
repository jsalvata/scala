// From http://www.google.com/url?sa=D&q=http://stackoverflow.com/questions/5961660/dynamic-trait-nosuchfielderror-2-9-0rc4&usg=AFQjCNG2ikAyLZc6AB6v1RG4vXDwMRWN1w
trait Dyn {
  val D1 = new Dynamic {
    def applyDynamic(args: Any*) = "Hi"
    def applyDynamic = "Hi0"
  }

  object D2 extends Dynamic {
    def applyDynamic(args: Any*) = "Hey"
    def applyDynamic = "Hey0"
  }
}

trait T { self: Dyn =>
  def foo1 = D1.X
  def foo2 = D2.X
}

object T extends T with Dyn
object Dyn extends Dyn

object Test extends App {
  T.D1.X // works
  Dyn.D1.X // works
  T.foo1 // doesn't work: java.lang.NoSuchFieldError: reflPoly$Cache1
  T.D2.X // works
  Dyn.D2.X // works
  T.foo2 // works 
}
