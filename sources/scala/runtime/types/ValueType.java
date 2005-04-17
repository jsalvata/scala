/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2003, LAMP/EPFL                  **
**  __\ \/ /__/ __ |/ /__/ __ |                                         **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */

// $Id$

package scala.runtime.types;

import scala.Type;
import scala.Array;

/**
 * Abstract superclass for all value types.
 *
 * @author Michel Schinz
 * @version 1.0
 */

abstract public class ValueType extends Type {
    public boolean isInstance(Object o) {
        throw new UnsupportedOperationException();
    }
    public boolean isSubType(Type that) {
        return that == Type.Any
            || that == Type.AnyVal
            || that == this;
    }
    public boolean isSameType(Type that) {
        return this == that;
    }
    public Array newArray(int size) {
        throw new Error("internal error (Scala runtime)");
    }
}
