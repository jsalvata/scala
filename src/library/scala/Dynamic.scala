/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2010-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

package scala

import scala.Dynamic.MethodName
import java.lang.AssertionError

/** A marker trait that enables dynamic invocations. Instances `x` of this
 *  trait allow calls `x.meth(args)` for arbitrary method names `meth` and
 *  argument lists `args`.  If a call is not natively supported, either
 *  because the visible compile-time type of x doesn't define method "meth"
 *  or because it doesn't make it visible, the call is rewritten to
 *  `x.applyDynamic(args)`.
 *
 *  `x.meth()` is rewritten to `x()`. `x.meth` is rewritten to `x`.
 *
 *  Unqualified calls (`meth(args)`) are never rewritten.
 *
 *  At least one applyDynamic method must be defined as soon as this
 *  trait is mixed in -- even into abstract classes or traits.
 *  If an applyDynamic method declares an implicit parameter of type
 *  `Dynamic.MethodName` and it resolves to Dynamic.callingName, the
 *  compiler will magically provide the name used in the original
 *  invocation (`"meth"`).
 *
 *  As of scala 2.9, `scalac` must receive the `-Xexperimental` option for
 *  `Dynamic` to receive this treatment.
 */
trait Dynamic

object Dynamic {
  class MethodName(private val name: String) {
    override def toString = name
  }

  /**This method magically returns the calling name used to invoke an applyDynamic method via the
   * scala.Dynamic mechanism.
   *
   * If invoked directly (without magic), it just throws an AssertionError.
   */
  implicit def callingName: MethodName = {
    throw new AssertionError("The scala.Dynamic mechanism failed to provide the calling method's name.")
  }
}