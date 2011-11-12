package scala.xml
import scala.collection.mutable.ListBuffer

/**
 * This unmarshaller supports XML literals compatible with the in-language XML
 * support prior to the introduction of unmarshallers.
 * 
 * It was built by transforming the grammar of the sequence of calls generated
 * by the compiler (which can be derived from the XmlExpr grammar in the Scala
 * Language Specification) into a regular tree grammar
 * (see http://wam.inrialpes.fr/courses/PG-MoSIG10/tree-grammars.pdf for an
 * introduction) with all productions in one of these forms:
 * 
 * A ::= a
 * A ::= bB
 * A ::= e[E]F
 * A ::= CD
 *
 * It is known that it is always possible to write a Regular Tree Grammar 
 * in this form (or even the more restrictive Chomsky Normal Form).
 * 
 * A class is then defined for each non-terminal (A, B, C...), with a method
 * for each production:
 *
 * `A ::= a`  is an end production, and translates to a method "a" which 
 *            returns whatever its outer class told it to or, if it's outer
 *            class is the one for the start symbol, returns the desired XML
 *            literal.
 *
 * `A ::= bB` translates to a method "b" which returns a value of class B.
 *
 * `A ::= e[E]F` translates to a method "e" which returns a value of
 *            a class E or, typically, an anonymous subclass of C,
 *            which knows how to return an F on its end productions.
 *
 * `A ::= CD` translates to the class for A extending that for C and
 *            providing it with a mechanism, typically a `next` method,
 *            to return a D on its end productions.
 *
 * The unmarshaller itself corresponds to the grammar's start symbol.
 *
 * TODO: review scaladoc formatting.
 * 
 * @author Jordi Salvat i Alabart
 */
object ScalaXMLUnmarshaller extends XMLUnmarshaller {

  /**
   * The initial symbol for the XmlExpr grammar
   */
  def startXmlExpr()= new XmlExpr()

  /** 
   * XmlExpr := XmlContent Elements0
   */
  class XmlExpr extends XmlContent with ContinuationPassingNodeFactory {
    override protected var scope: NamespaceBinding= TopScope
    override protected type E[N <: Node]= Elements0[N]
    override protected def next[N <: Node](node: N) = new Elements0[N](node)
  }

  /**
   * Objects of this trait produce nodes which must be processed
   * in a common way (the `next` method) but return a type dependent on the
   * type of the node created.
   */
  trait ContinuationPassingNodeFactory {
    protected type E[_ <: Node]
    protected def next[N <: Node](node: N): E[N]
  }

  /**
   * This class implements the productions:
   *
   * XmlContent ::= cdStart XmlContent // ignore cdStart
   * XmlContent ::= charData // this implements `XmlExpr ::= CData` by ignoring cdStart and, elsewhere, cdEnd too
   * XmlContent ::= cdEnd XmlContent // ignore cdEnd -- for when XmlContent is the "elsewhere"
   * XmlContent ::= pi
   * XmlContent ::= comment
   */
  trait XmlContent extends RealElement {
    this: ContinuationPassingNodeFactory =>
    def cdStart(): this.type = this // ignore
    def charData(text: String) = next(new Text(text))
    def cdEnd(): this.type = this // ignore
    def pi(target: String, text: String) = next(new ProcInstr(target, text))
    def comment(text: String) = next(new Comment(text))
  }

  /**
   * RealElement ::= sTag[Element]
   *   including the (undocumented) special sTags xml:group and xml:unparsed, which
   *   have different semantics (must create and return a different type of node).
   */
  trait RealElement extends Dynamic {
    outer: ContinuationPassingNodeFactory =>
      
    protected var scope: NamespaceBinding
    
    def applyDynamic(name: String)() = new Element(elementQName(name), scope) {
      def eTag() = outer.next(createElement)
    }
    def `sTag_xml:group`() = new Element("xml:group", scope) {
      def eTag() = outer.next(createGroup)
    }
    def `sTag_xml:unparsed`() = new Element("xml:unparsed", scope) {
      def eTag() = outer.next(createUnparsed)
    }    
  }

  /**
   * This class implements the productions:
   *
   * (XmlContent) Elements0 ::= endXmlExpr
   *   returns specific type of Node corresponding to XmlContent
   *
   * Elements0 ::= cdEnd Elements0 // ignore cdEnd (see comments in XmlContent) 
   * Elements0 ::= RealElement Elements
   */
  class Elements0[N <: Node](node: N) extends RealElement with ContinuationPassingNodeFactory {
    override protected var scope: NamespaceBinding= TopScope
    override type E[N <: Node]= Elements
    override protected def next[N <: Node](newNode: N) = new Elements(node, newNode)

    def endXmlExpr() = node
    def cdEnd(): this.type = this // ignore
  }

  /**
   * (xmlContent element...) Elements ::= endXmlExpr
   *   returns Seq[Node]
   * 
   * Elements ::= RealElement Elements
   */
  class Elements(node1: Node, node2: Node) extends RealElement with BufferedUnmarshaller {
    override protected var scope: NamespaceBinding= TopScope
    buf &+ node1
    buf &+ node2

    def endXmlExpr() = buf.toList
  }

  /**
   * Content ::= charData Content
   * Content ::= XmlContent Content
   * Content ::= entityRef Content
   * Content ::= scalaExpr Content
   */
  trait Content extends XmlContent with BufferedUnmarshaller {
    override def charData(text: String): this.type = {
      if (text != "" ) next(new Text(text)) // for backward compatiblity -- the conditional was in makeXMLseq
      else this
    }
    def entityRef(name: String): this.type = next(new EntityRef(name))
    def scalaExpr(expression: Any): this.type = next(expression)
  }

  /**
   * Element ::= startAttributes[Attributes] Element
   * Element ::= Content Element
   */
  abstract class Element(qname: String, parentScope: NamespaceBinding) extends Content with Dynamic {
    this_element =>

    private var attributes= identity[MetaData] _
    override protected var scope: NamespaceBinding= parentScope

    def createElement = {
      val (prefix, localName)= splitPrefix(qname)
      new Elem(prefix.orNull, localName, attributes(Null), scope, buf.toList: _*)
    }
    def createGroup = new Group(buf.toList)
    def createUnparsed = new Unparsed(buf.text)

    def startAttributes[E >: this.type <: this.type]() = new Attributes {
      def endAttributes(): E = this_element 
    }

    /**
     * Attributes ::= startAttribute[Attribute] Attributes
     */
    class Attributes extends Dynamic {
      this_attributes =>

      def applyDynamic[E >: this.type <: this.type](name: String)() = new Attribute[E](attributeQName(name))

      /**
       * Attribute ::= charData Attribute
       * Attribute ::= entityRef Attribute
       * Attribute ::= scalaExpr AnyRef
       */
      class Attribute[A >: this.type <: this.type](qName: String) extends BufferedUnmarshaller {
        def addAttribute(attribute: MetaData => MetaData) = {
          this_element.attributes = this_element.attributes compose attribute
        }
  
        /**
         * Process this attribute and register it as a plain attribute or as a
         * namespace binding.
         */
        def processAttribute(value: Seq[Node]) {
          splitPrefix(qName) match {
            case (Some("xmlns"), prefix) => scope= new NamespaceBinding(prefix, value.text, scope)
            case (None, "xmlns") => scope= new NamespaceBinding(null, value.text, scope)
            case (Some(ns), localName) => addAttribute(new PrefixedAttribute(ns, localName, value, _))
            case (None, localName) =>  addAttribute(new UnprefixedAttribute(localName, value, _))
          }
        }
  
        def scalaExpr(expression: String) = {
          val value= if (expression != null) Text(expression) else null: NodeSeq
          processAttribute(value)
          new AnyRef {
            def endAttribute(): A = this_attributes
          }
        }
  
        def scalaExpr(expression: Option[Seq[Node]]) = {
          processAttribute(expression.orNull)
          new AnyRef {
            def endAttribute(): A = this_attributes
          }
        }
  
        def scalaExpr(expression: Seq[Node]) = {
          processAttribute(expression)
          new AnyRef {
            def endAttribute(): A = this_attributes
          }
        }
  
        def endAttribute(): A = {
          buf.length match { // for backward compatibility -- this switch was in parseAttribute
            case 0 => processAttribute(Nil)
            case 1 => processAttribute(buf.head)
            case _ => processAttribute(buf)
          }
          this_attributes
        }

        def charData(text: String): this.type = {
          if (text != "" ) next(new Text(text)) // for backward compatiblity -- the conditional was in makeXMLseq
          else this
        }
        def entityRef(name: String): this.type = next(new EntityRef(name))
      }
    }
  }

  trait BufferedUnmarshaller extends ContinuationPassingNodeFactory {
    override type E[N]= this.type
    override protected def next[N <: Node](newElement: N): this.type = next(newElement:Any)

    protected val buf= new NodeBuffer()
    protected def next(newElement: Any): this.type = {
      buf &+ newElement
      this
    }
  }

  /**
   * The initial symbol for the XmlPattern grammar
   * 
   * We're using ContentP instead of ElementPattern because it just defines more
   * productions -- which the parser will simply not use.
   */
  def startXmlPattern() = new ContentP(null, null) {
    def matches(nodes: Seq[Node]) = Some((Nil, nodes))
  }

  abstract class ContentP(protected val parent: ContentP, protected val qName: String) extends Dynamic {
    outer =>

    /**
     * @returns Some((predecessor values :: parent's predecessor values, remaining nodes)) if the given sequence matches
     * this pattern -- None otherwise.
     */
    def matches(nodes: Seq[Node]): Option[(Seq[Any], Seq[Node])]

    def applyDynamic(name: String)() = new ContentP(outer, elementQName(name)) {
      def matches(nodes: Seq[Node]) = Some((Nil, nodes)) 
    }

    def startAttributes(): this.type = this
    def endAttributes(): this.type = this

    def eTag() = new ContentP(parent.parent, parent.qName) {
      private def sameName(n: Node) = splitPrefix(outer.qName) match {
        case (None, localName) => n.label == localName
        case (Some(prefix), localName) => n.prefix == prefix && n.label == localName
      }

      def matches(nodes: Seq[Node]) = outer.parent.matches(nodes) match {
        case Some((parentValues, parentNode :: parentRest)) if sameName(parentNode) => 
          outer.matches(parentNode.child) match {
            case Some((childValues, Nil)) => 
              Some((
                  parentValues ++ childValues,
                  parentRest))
            case _ => None
          }
        case _ => None
      }
    }

    def scalaPattern() = new ContentP(parent, qName) {
      def matches(nodes: Seq[Node]) = outer.matches(nodes) match {
          case Some((values, node :: rest)) => Some((values :+ node, rest))
          case _ => None
      }
    }

    def scalaStarPattern() = new ContentP(parent, qName) {
      def matches(nodes: Seq[Node]) = outer.matches(nodes) match {
          case Some((values, nodes)) => Some((values :+ nodes, Nil))
          case _ => None
      }
    }

    def charData(text: String) = new ContentP(parent, qName) {
      override def matches(nodes: Seq[Node]) = outer.matches(nodes) match {
        case Some((values, Text(txt) :: rest)) if txt == text => Some((values, rest))
        case _ => None
      }
    }

    lazy val endXmlPattern = new AnyRef {
      def unapplySeq(n: Node) = outer.matches(List(n)) match {
        case Some((values, List())) => Some(values)
        case _ => None
      }
    }
  }

  /** Returns (Some(prefix) | None, rest) based on position of ':' */
  private def splitPrefix(name: String): (Option[String], String) = name.split(":", 2) match {
    case Array(pre, rest)  => (Some(pre), rest)
    case _                 => (None, name)
  }
  
  private def elementQName(methodName: String) = qName(methodName, "sTag")

  private def attributeQName(methodName: String) = qName(methodName, "startAttribute") 
  
  private def qName(methodName: String, prefix: String) = methodName.split("_", 2) match {
    case Array(prefix, qName) => qName
    case _ => throw new java.lang.AssertionError(methodName+" is not a valid "+prefix+" method name.")
  }
}