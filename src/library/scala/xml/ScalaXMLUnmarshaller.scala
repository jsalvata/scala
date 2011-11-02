package scala.xml

object ScalaXMLUnmarshaller extends XMLUnmarshaller {

  def startDocument()= new StartUnmarshaller()

  trait NamespaceHandler {
    this_handler =>

    protected var scope: NamespaceBinding 

    def startPrefixMapping(prefix: Option[String]) = new BufferedUnmarshaller {
      def embeddedExpression(expression: Any) = expression match {
        case str: String => next(new Text(str))
        case _ => next(expression)
      }
      def endPrefixMapping(): this_handler.type = {
        scope = new NamespaceBinding(prefix.orNull, buf.map(_.toString).fold("")(_+_), scope)
        this_handler
      }
    }
  }

  /** Unmarshaller for XmlExpr (see ScalaReference 10.1)
    */
  class StartUnmarshaller extends ElementPattern with NamespaceHandler {
    this_start =>

    override protected var scope: NamespaceBinding= TopScope

    private def next[N <: Node](node: N) = new FirstNodeUnmarshaller[N](node)
  
    // Some of these are repeated, but the generic solution I found was much worse 
    def startCDATA() = this // ignore
    def characters(text: String) = next(new Text(text))
    def processingInstruction(target: String, text: String) = next(new ProcInstr(target, text))
    def comment(text: String) = next(new Comment(text))
  
    def unparsed(str: String) = next(new Unparsed(str))
    def startGroup() = new ContentUnmarshaller(scope) {
      def endGroup() = this_start.next(new Group(buf))
    }
    def startElement(prefix: Option[String], localName: String) =
        new ElementUnmarshaller(prefix, localName, scope) {
      def endElement() = this_start.next(createElement)
    }
  }

  /** Unmarshaller for XmlContent.
    */
  class FirstNodeUnmarshaller[N <: Node](node: N) extends ElementPattern with NamespaceHandler {
    this_firstNode =>

    override protected var scope: NamespaceBinding= TopScope

    def endDocument() = node
  
    private def next(newElement: Node) = new SeqUnmarshaller(node, newElement)

    // Some of these are repeated, but the generic solution I found was much worse 
    def endCDATA() = this // ignore

    def unparsed(str: String) = next(new Unparsed(str))
    def startGroup() = new ContentUnmarshaller(scope) {
      def endGroup() = this_firstNode.next(new Group(buf))
    }
    def startElement(prefix: Option[String], localName: String) =
        new ElementUnmarshaller(prefix, localName, scope) {
      def endElement() = this_firstNode.next(createElement)
    }
  }

  /** Unmarshaller for XmlContent {Element}
    */
  class SeqUnmarshaller(node1: Node, node2: Node) extends ContentUnmarshaller(TopScope) {
    buf &+ node1
    buf &+ node2

    def endDocument() = buf.toList
  }

  /** Unmarshaller for the content of an Element.
    */
  class ContentUnmarshaller(parentScope: NamespaceBinding) extends BufferedUnmarshaller with ElementPattern with NamespaceHandler {
    this_content =>
      
    override protected var scope: NamespaceBinding= parentScope

    override protected def next(newElement: Any): this.type = {
      scope= parentScope
      super.next(newElement)
    }

    // Some of these are repeated, but the generic solution I found was much worse 
    def comment(text: String): this.type = next(new Comment(text))
    def startCDATA(): this.type = this // ignore
    def endCDATA(): this.type = this // ignore
    def processingInstruction(target: String, text: String): this.type = next(new ProcInstr(target, text))

    def embeddedExpression(expression: Any): this.type = next(expression)
    
    def unparsed(str: String): this.type = next(new Unparsed(str))
    // TODO: Type parameter E is a workaround for SI-5130. Remove it when that is fixed:
    // https://issues.scala-lang.org/browse/SI-5130?focusedCommentId=55187#comment-55187
    def startGroup[E >: this.type <: this.type]() = new ContentUnmarshaller(scope) {
      def endGroup(): E = this_content.next(new Group(buf))
    }
    // TODO: Type parameter E is a workaround for SI-5130. Remove it when that is fixed:
    // https://issues.scala-lang.org/browse/SI-5130?focusedCommentId=55187#comment-55187
    def startElement[E >: this.type <: this.type](prefix: Option[String], localName: String) =
        new ElementUnmarshaller(prefix, localName, scope) {
      def endElement(): E = this_content.next(createElement)
    }
  }

  /** Unmarshaller for Element (see ScalaReference 10.1)
    */
  abstract class ElementUnmarshaller(prefix: Option[String], localName: String, scope: NamespaceBinding) extends ContentUnmarshaller(scope) {
    this_element =>

    private var attributes: MetaData = Null

    def createElement = new Elem(prefix.orNull, localName, attributes, scope, buf: _*)

    def startAttribute(prefix: Option[String], localName: String) =
      new AttributeUnmarshaller[this.type](prefix, localName)

    // TODO: Type parameter E is a workaround for SI-5130. Remove it when that is fixed:
    // https://issues.scala-lang.org/browse/SI-5130?focusedCommentId=55187#comment-55187
    class AttributeUnmarshaller[E >: this.type](prefix: Option[String], localName: String) extends BufferedUnmarshaller {
      def storeAttribute(value: Seq[Node]) {
        this_element.attributes= prefix match {
          case Some(ns) => new PrefixedAttribute(ns, localName, value, this_element.attributes)
          case None =>  new UnprefixedAttribute(localName, value, this_element.attributes)
        }
      }

      def embeddedExpression(expression: String) = {
        val value= if (expression != null) Text(expression) else null: NodeSeq
        storeAttribute(value)
        new AnyRef {
          def endAttribute(): E = this_element
        }
      }

      def embeddedExpression(expression: Option[Seq[Node]]) = {
        storeAttribute(expression.orNull)
        new AnyRef {
          def endAttribute(): E = this_element
        }
      }

      def embeddedExpression(expression: Seq[Node]) = {
        storeAttribute(expression)
        new AnyRef {
          def endAttribute(): E = this_element
        }
      }

      def endAttribute(): E = {
        buf.length match { // for backward compatibility -- this switch was in parseAttribute
          case 0 => storeAttribute(Nil)
          case 1 => storeAttribute(buf.head)
          case _ => storeAttribute(buf)
        }
        this_element
      }
    }
  }

  abstract class BufferedUnmarshaller {
    protected val buf= new NodeBuffer()  

    protected def next(newElement: Any): this.type = {
      buf &+ newElement
      this
    }

    def characters(text: String): this.type = {
      if (text != "" ) next(new Text(text)) // for backward compatiblity -- the conditional was in makeXMLseq
      else this
    }
    def skippedEntity(name: String): this.type = next(new EntityRef(name))
  }

  trait ElementPattern {
    val elementPattern = new AnyRef {
      def unapplySeq(n: Node) = n match {
        case _: SpecialNode | _: Group  => None
        case _                          => Some((n.prefix, n.label, n.attributes, n.scope, n.child))
      }
    }
  }  
}
