package scala.xml

/**
 * This unmarshaller supports XML literals compatible with the in-language XML
 * support prior to the introduction of unmarshallers.
 * 
 * It was built by transforming the grammar of the sequence of calls generated
 * by the compiler (which can be derived from the XmlExpr grammar in the Scala
 * Language Specification) into a regular tree grammar
 * (see http://wam.inrialpes.fr/courses/PG-MoSIG10/tree-grammars.pdf for an
 * introduction) in Greibach Normal Form -- i.e., with all productions in form:
 * 
 * A ::= a
 * A ::= bB
 *  or
 * A ::= c[C]D
 *
 * A class is then defined for each non-terminal (A, B, C...), with a method
 * for each production:
 *
 * `A ::= yB` translates to a method "y" which returns a value of class B.
 *
 * `A ::= y[C]D` translates to a method "y" which returns a value of
 *            a class C or, typically, an anonymous subclass of C,
 *            which knows how to return a D on its end productions.
 *
 * `A ::= a`  is an end production, and translates to a method "a" which 
 *            returns whatever its outer class told it to or, for the start 
 *            symbol, returns the desired XML literal.
 *
 * A method startS is defined to obtain an object of class S corresponding to
 * the grammar's start symbol.
 * 
 * @author Jordi Salvat i Alabart
 */
object ScalaXMLUnmarshaller extends XMLUnmarshaller {

  /**
   * The initial symbol for the XmlExpr grammar
   */
  def startXmlExpr()= new XmlExpr()

  /**
   * The initial symbol for the XmlPattern grammar
   */
  def startXmlPattern() = new ElementPattern()
 
  /** 
   * This class implements the production
   *   XmlExpr := XmlContent Elements0
   * which explodes into:
   *
   * XmlExpr ::= sTag[Element] Elements0
   * XmlExpr ::= charData Elements0 // this implements `XmlExpr ::= CData Elements0` by ignoring cdStart and cdEnd
   * XmlExpr ::= pi Elements0
   * XmlExpr ::= comment Elements0
   *
   * TODO: review scaladoc formatting.
   */
  class XmlExpr extends Dynamic {
    this_xmlExpr =>

    private def next[N <: Node](node: N) = new Elements0[N](node)
  
    def applyDynamic(name: String)() = new Element(elementQName(name), TopScope) {
      def eTag() = this_xmlExpr.next(createElement)
    }
    def `sTag_xml:group`() = new Element("xml:group", TopScope) {
      def eTag() = this_xmlExpr.next(createGroup)
    }
    def `sTag_xml:unparsed`() = new Element("xml:unparsed", TopScope) {
      def eTag() = this_xmlExpr.next(createUnparsed)
    }
    def cdStart() = this // ignore
    def pi(target: String, text: String) = next(new ProcInstr(target, text))
    def charData(text: String) = next(new Text(text))
    def comment(text: String) = next(new Comment(text))
  }

  /**
   * This class implements the productions:
   *
   * (XmlContent) Elements0 ::= endXmlExpr
   *   returns specific subtype of Node corresponding to XmlContent
   * 
   * Elements0 ::= sTag[Element] Elements
   */
  class Elements0[N <: Node](node: N) extends Dynamic {
    this_xmlContent =>

    def endXmlExpr() = node
  
    def applyDynamic(name: String)() = new Element(elementQName(name), TopScope) {
      def eTag() = new Elements(node, createElement)
    }
    def `sTag_xml:group`() = new Element("xml:group", TopScope) {
      def eTag() = new Elements(node, createGroup)
    }
    def `sTag_xml:unparsed`() = new Element("xml:unparsed", TopScope) {
      def eTag() = new Elements(node, createUnparsed)
    }
    def cdEnd() = this // ignore
  }

  /**
   * This class implements the productions:
   *
   * (xmlContent element...) Elements ::= endXmlExpr
   *   returns Seq[Node]
   * 
   * Elements ::= startElement[Element] Elements
   * 
   * The implementation of the later reuses Content, which includes other
   * productions, but those are never used because the parser limits it.
   */
  class Elements(node1: Node, node2: Node) extends Content {
    buf &+ node1
    buf &+ node2
    override protected var scope: NamespaceBinding= TopScope

    def endXmlExpr() = buf.toList
  }

  /**
   * This class implements the productions:
   *
   * Content ::= scalaExpr Content
   * Content ::= startElement[Element] Content
   * Content ::= pi Content
   * Content ::= comment Content
   * 
   * And also, via BufferedUnmarshaller:
   * 
   * Content ::= charData Content
   * Content ::= entityRef Content
   * Content ::= CDSect Content // by ignoring cdStart and cdEnd
   */
  abstract class Content extends BufferedUnmarshaller with Dynamic {
    this_content =>

    protected var scope: NamespaceBinding

    def scalaExpr(expression: Any): this.type = next(expression)
    // TODO: Type parameter E is a workaround for SI-5130. Remove it when that is fixed:
    // https://issues.scala-lang.org/browse/SI-5130?focusedCommentId=55187#comment-55187
    def applyDynamic[E >: this.type <: this.type](name: String)() = new Element(elementQName(name), TopScope) {
      def eTag(): E = this_content.next(createElement)
    }
    def `sTag_xml:group`[E >: this.type <: this.type]() = new Element("xml:group", TopScope) {
      def eTag() = this_content.next(createGroup)
    }
    def `sTag_xml:unparsed`[E >: this.type <: this.type]() = new Element("xml:unparsed", TopScope) {
      def eTag() = this_content.next(createUnparsed)
    }
    def pi(target: String, text: String): this.type = next(new ProcInstr(target, text))
    def comment(text: String): this.type = next(new Comment(text))

    def cdStart(): this.type = this // ignore
    def cdEnd(): this.type = this // ignore
  }

  /**
   * This class implements the production:
   * 
   * Element ::= startAttributes[Attributes] Element
   *
   * and, via Content:
   * 
   * Element ::= scalaExpr Element
   * Element ::= startElement[Element] Element
   * Element ::= pi Element
   * Element ::= comment Element
   * Element ::= charData Element
   * Element ::= entityRef Element
   * Element ::= CDSect Element
   */
  abstract class Element(qname: String, parentScope: NamespaceBinding) extends Content with Dynamic {
    this_element =>

    private var attributes= identity[MetaData] _
    override protected var scope: NamespaceBinding= parentScope

    def createElement = {
      val (prefix, localName)= splitPrefix(qname)
      new Elem(prefix.orNull, localName, attributes(Null), scope, buf: _*)
    }
    def createGroup = new Group(buf)
    def createUnparsed = new Unparsed(buf.text)

    def startAttributes[E >: this.type <: this.type]() = new Attributes {
      def endAttributes(): E = this_element 
    }

    /**
     * This class implements the production:
     * 
     * Attributes ::= startAttribute[Attribute] Attributes
     */
    class Attributes extends Dynamic {
      this_attributes =>

      def applyDynamic[E >: this.type <: this.type](name: String)() = new Attribute[E](attributeQName(name))

      /**
       * This class implements the production:
       * 
       * Attribute ::= scalaExpr AnyRef
       * 
       * And also, via BufferedUnmarshaller:
       *
       * Attribute ::= charData Attribute
       * Attribute ::= entityRef Attribute
       */
      class Attribute[E >: this.type <: this.type](qName: String) extends BufferedUnmarshaller {
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
            def endAttribute(): E = this_attributes
          }
        }
  
        def scalaExpr(expression: Option[Seq[Node]]) = {
          processAttribute(expression.orNull)
          new AnyRef {
            def endAttribute(): E = this_attributes
          }
        }
  
        def scalaExpr(expression: Seq[Node]) = {
          processAttribute(expression)
          new AnyRef {
            def endAttribute(): E = this_attributes
          }
        }
  
        def endAttribute(): E = {
          buf.length match { // for backward compatibility -- this switch was in parseAttribute
            case 0 => processAttribute(Nil)
            case 1 => processAttribute(buf.head)
            case _ => processAttribute(buf)
          }
          this_attributes
        }
      }
    }
  }

  /**
   * This class supports productions "X" with a sequence of content
   * (Content, Element, and Attribute), and defines the following
   * productions common to them:
   * 
   * X ::= charData X
   * X ::= entityRef X
   */
  abstract class BufferedUnmarshaller {
    protected val buf= new NodeBuffer()  

    protected def next(newElement: Any): this.type = {
      buf &+ newElement
      this
    }

    def charData(text: String): this.type = {
      if (text != "" ) next(new Text(text)) // for backward compatiblity -- the conditional was in makeXMLseq
      else this
    }
    def entityRef(name: String): this.type = next(new EntityRef(name))
  }

  /**
   * ElementPattern := 
   */
  class ElementPattern {
    def sTag(qName: String) = this
    def eTag()= this
    def scalaExpr(expression: Any) = this

    def endXmlPattern = new AnyRef {
      }
  }  
  def unapplySeq(n: Node) = n match {
    case _: SpecialNode | _: Group  => None
    case _ => Some((n.prefix, n.label, n.attributes, n.scope, n.child))
  }

  /** Returns (Some(prefix) | None, rest) based on position of ':' */
  private def splitPrefix(name: String): (Option[String], String) = name.split(":", 2) match {
    case Array(pre, rest)  => (Some(pre), rest)
    case _                 => (None, name)
  }
  
  private def elementQName(methodName: String) = qName(methodName, "sTag_")

  private def attributeQName(methodName: String) = qName(methodName, "startAttribute_") 
  
  private def qName(methodName: String, prefix: String) = methodName.split("_", 2) match {
    case Array(`methodName`, qName) => qName
    case _ => throw new java.lang.AssertionError(methodName+" is not a valid "+methodName+" method name.")
  } 
}
