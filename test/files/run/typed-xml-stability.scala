import scala.xml.{NodeSeq, Elem, Text}
import scala.collection._

/**
 * This test compares a bunch of XML literals (extracted from various
 * other tests) with their rendering in scala.xml as per scala 2.9 (code
 * created by compiling the 1st bit with -Xprint:parser).
 *
 * The intent is to check for backward compatiblity in the implementation
 * of typed XML.
 */
object Test extends App {
  val x = "x"
  val xs = List(1, 2, 3)

  val xmlLiterals= List[NodeSeq](
    <cat>dog</cat>,
    <bks>
      <title>Blabla</title>
      <title>Blubabla</title>
      <title>Baaaaaaalabla</title>
    </bks>,
    <title>Blabla</title>,
    <br/><br/>,
    <br/>,
    <span class='clicksentence' style={if(true) "background-color: yellow" else ""}>{x}</span>,
    <span class='clicksentence' style={if(false) "background-color: yellow" else ""}>{x}</span>,
    <a xmlns=""/>,
    <unmatchedType>
      <theType>
	{"unmatched class"}
      </theType>
      <theValue>
	{"unmatched"}
      </theValue>
    </unmatchedType>,
    <a/>,
    <b/>,
    <a>{}</a>,
    <div>{ x }</div>,
    <span>{"louenesee"}</span>,
    <div/>,
    <x><x><x><x><x><x><x><x><x><x><x><x><x><x><x><x><x><x>{ "foo" }</x></x></x></x></x></x></x></x></x></x></x></x></x></x></x></x></x></x>,
    <t user:tag=""/>,
    <t user:tag="X"/>,
    <root xmlns:ns="nsUri" ns:at="rootVal"><sub ns:at="subVal"/></root>,
    <foo/>,
    <p><lost/><t><s><r></r></s></t></p>,
    <root>
      <subnode>
	<version>1</version>
      </subnode>
      <contents>
	<version>1</version>
      </contents>
    </root>,
    <html>
      <head><title>Scala</title></head>
      <body>{xs}</body>
    </html>,
    <code>
    
      class xyz[A] extends TypeConstraint
      
      def loopWhile[T](cond: =>Boolean)(body: =>(Unit @xyz[T])): Unit @ xyz[T] = {{
	if (cond) {{
	  body
	  loopWhile[T](cond)(body)
	}}
      }}

      def test() = {{
	var x = 7
	loopWhile(x != 0) {{
	  x = x - 1
	  (): @xyz[Int]
	}}
      }}
      
    </code>,
    <foo a="1" b="2" c="3" d="4" e={"5"} />,
    <entry>
    {
      for (item <- "a,b,c" split ',') yield
	<elem>{ item }</elem>
    }
    </entry>,
    <k a="1" b="2"/>,
    <k a="1"/>,
    <xml:group><p><lost/><t><s><r></r></s></t></p></xml:group>,
    <a></a>,
    <a/>,
    <a>{ xml.NodeSeq.Empty }</a>,
    <a>{""}</a>,
    <a>{ if (true) "" else "I like turtles" }</a>,
    <wsdl:definitions name={x} xmlns:tns = { "target1" } >
    </wsdl:definitions>,
    <wsdl:definitions name={x} xmlns:tns = { Text("target3") } >
    </wsdl:definitions>
  )

  val originalXml = List[NodeSeq]({
      {
        new _root_.scala.xml.Elem(null, "cat", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(new _root_.scala.xml.Text("dog"));
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "bks", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "title", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus(new _root_.scala.xml.Text("Blabla"));
                $buf
              }: _*))
            }
          });
          $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "title", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus(new _root_.scala.xml.Text("Blubabla"));
                $buf
              }: _*))
            }
          });
          $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "title", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus(new _root_.scala.xml.Text("Baaaaaaalabla"));
                $buf
              }: _*))
            }
          });
          $buf.$amp$plus(new _root_.scala.xml.Text("\012    "));
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "title", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(new _root_.scala.xml.Text("Blabla"));
          $buf
        }: _*))
      }
    }, {
      val $buf = new _root_.scala.xml.NodeBuffer();
      $buf.$amp$plus({
        {
          new _root_.scala.xml.Elem(null, "br", _root_.scala.xml.Null, $scope)
        }
      });
      $buf.$amp$plus({
        {
          new _root_.scala.xml.Elem(null, "br", _root_.scala.xml.Null, $scope)
        }
      });
      $buf
    }, {
      {
        new _root_.scala.xml.Elem(null, "br", _root_.scala.xml.Null, $scope)
      }
    }, {
      {
        var $md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
        $md = new _root_.scala.xml.UnprefixedAttribute("style", if (true)
          "background-color: yellow"
        else
          "", $md);
        $md = new _root_.scala.xml.UnprefixedAttribute("class", new _root_.scala.xml.Text("clicksentence"), $md);
        new _root_.scala.xml.Elem(null, "span", $md, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(x);
          $buf
        }: _*))
      }
    }, {
      {
        var $md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
        $md = new _root_.scala.xml.UnprefixedAttribute("style", if (false)
          "background-color: yellow"
        else
          "", $md);
        $md = new _root_.scala.xml.UnprefixedAttribute("class", new _root_.scala.xml.Text("clicksentence"), $md);
        new _root_.scala.xml.Elem(null, "span", $md, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(x);
          $buf
        }: _*))
      }
    }, {
      var $tmpscope: _root_.scala.xml.NamespaceBinding = $scope;
      $tmpscope = new _root_.scala.xml.NamespaceBinding(null, null, $tmpscope);
      {
        val $scope: _root_.scala.xml.NamespaceBinding = $tmpscope;
        new _root_.scala.xml.Elem(null, "a", _root_.scala.xml.Null, $scope)
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "unmatchedType", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "theType", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus(new _root_.scala.xml.Text("\012\011"));
                $buf.$amp$plus("unmatched class");
                $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
                $buf
              }: _*))
            }
          });
          $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "theValue", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus(new _root_.scala.xml.Text("\012\011"));
                $buf.$amp$plus("unmatched");
                $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
                $buf
              }: _*))
            }
          });
          $buf.$amp$plus(new _root_.scala.xml.Text("\012    "));
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "a", _root_.scala.xml.Null, $scope)
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "b", _root_.scala.xml.Null, $scope)
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "a", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(());
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "div", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(x);
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "span", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus("louenesee");
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "div", _root_.scala.xml.Null, $scope)
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus({
                  {
                    new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                      val $buf = new _root_.scala.xml.NodeBuffer();
                      $buf.$amp$plus({
                        {
                          new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                            val $buf = new _root_.scala.xml.NodeBuffer();
                            $buf.$amp$plus({
                              {
                                new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                  val $buf = new _root_.scala.xml.NodeBuffer();
                                  $buf.$amp$plus({
                                    {
                                      new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                        val $buf = new _root_.scala.xml.NodeBuffer();
                                        $buf.$amp$plus({
                                          {
                                            new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                              val $buf = new _root_.scala.xml.NodeBuffer();
                                              $buf.$amp$plus({
                                                {
                                                  new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                                    val $buf = new _root_.scala.xml.NodeBuffer();
                                                    $buf.$amp$plus({
                                                      {
                                                        new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                                          val $buf = new _root_.scala.xml.NodeBuffer();
                                                          $buf.$amp$plus({
                                                            {
                                                              new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                                                val $buf = new _root_.scala.xml.NodeBuffer();
                                                                $buf.$amp$plus({
                                                                  {
                                                                    new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                                                      val $buf = new _root_.scala.xml.NodeBuffer();
                                                                      $buf.$amp$plus({
                                                                        {
                                                                          new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                                                            val $buf = new _root_.scala.xml.NodeBuffer();
                                                                            $buf.$amp$plus({
                                                                              {
                                                                                new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                                                                  val $buf = new _root_.scala.xml.NodeBuffer();
                                                                                  $buf.$amp$plus({
                                                                                    {
                                                                                      new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                                                                        val $buf = new _root_.scala.xml.NodeBuffer();
                                                                                        $buf.$amp$plus({
                                                                                          {
                                                                                            new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                                                                              val $buf = new _root_.scala.xml.NodeBuffer();
                                                                                              $buf.$amp$plus({
                                                                                                {
                                                                                                  new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                                                                                    val $buf = new _root_.scala.xml.NodeBuffer();
                                                                                                    $buf.$amp$plus({
                                                                                                      {
                                                                                                        new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                                                                                          val $buf = new _root_.scala.xml.NodeBuffer();
                                                                                                          $buf.$amp$plus({
                                                                                                            {
                                                                                                              new _root_.scala.xml.Elem(null, "x", _root_.scala.xml.Null, $scope, ({
                                                                                                                val $buf = new _root_.scala.xml.NodeBuffer();
                                                                                                                $buf.$amp$plus("foo");
                                                                                                                $buf
                                                                                                              }: _*))
                                                                                                            }
                                                                                                          });
                                                                                                          $buf
                                                                                                        }: _*))
                                                                                                      }
                                                                                                    });
                                                                                                    $buf
                                                                                                  }: _*))
                                                                                                }
                                                                                              });
                                                                                              $buf
                                                                                            }: _*))
                                                                                          }
                                                                                        });
                                                                                        $buf
                                                                                      }: _*))
                                                                                    }
                                                                                  });
                                                                                  $buf
                                                                                }: _*))
                                                                              }
                                                                            });
                                                                            $buf
                                                                          }: _*))
                                                                        }
                                                                      });
                                                                      $buf
                                                                    }: _*))
                                                                  }
                                                                });
                                                                $buf
                                                              }: _*))
                                                            }
                                                          });
                                                          $buf
                                                        }: _*))
                                                      }
                                                    });
                                                    $buf
                                                  }: _*))
                                                }
                                              });
                                              $buf
                                            }: _*))
                                          }
                                        });
                                        $buf
                                      }: _*))
                                    }
                                  });
                                  $buf
                                }: _*))
                              }
                            });
                            $buf
                          }: _*))
                        }
                      });
                      $buf
                    }: _*))
                  }
                });
                $buf
              }: _*))
            }
          });
          $buf
        }: _*))
      }
    }, {
      {
        var $md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
        $md = new _root_.scala.xml.PrefixedAttribute("user", "tag", immutable.Nil, $md);
        new _root_.scala.xml.Elem(null, "t", $md, $scope)
      }
    }, {
      {
        var $md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
        $md = new _root_.scala.xml.PrefixedAttribute("user", "tag", new _root_.scala.xml.Text("X"), $md);
        new _root_.scala.xml.Elem(null, "t", $md, $scope)
      }
    }, {
      var $tmpscope: _root_.scala.xml.NamespaceBinding = $scope;
      $tmpscope = new _root_.scala.xml.NamespaceBinding("ns", "nsUri", $tmpscope);
      {
        val $scope: _root_.scala.xml.NamespaceBinding = $tmpscope;
        var $md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
        $md = new _root_.scala.xml.PrefixedAttribute("ns", "at", new _root_.scala.xml.Text("rootVal"), $md);
        new _root_.scala.xml.Elem(null, "root", $md, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus({
            {
              var $md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
              $md = new _root_.scala.xml.PrefixedAttribute("ns", "at", new _root_.scala.xml.Text("subVal"), $md);
              new _root_.scala.xml.Elem(null, "sub", $md, $scope)
            }
          });
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "foo", _root_.scala.xml.Null, $scope)
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "p", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "lost", _root_.scala.xml.Null, $scope)
            }
          });
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "t", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus({
                  {
                    new _root_.scala.xml.Elem(null, "s", _root_.scala.xml.Null, $scope, ({
                      val $buf = new _root_.scala.xml.NodeBuffer();
                      $buf.$amp$plus({
                        {
                          new _root_.scala.xml.Elem(null, "r", _root_.scala.xml.Null, $scope)
                        }
                      });
                      $buf
                    }: _*))
                  }
                });
                $buf
              }: _*))
            }
          });
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "root", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "subnode", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus(new _root_.scala.xml.Text("\012\011"));
                $buf.$amp$plus({
                  {
                    new _root_.scala.xml.Elem(null, "version", _root_.scala.xml.Null, $scope, ({
                      val $buf = new _root_.scala.xml.NodeBuffer();
                      $buf.$amp$plus(new _root_.scala.xml.Text("1"));
                      $buf
                    }: _*))
                  }
                });
                $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
                $buf
              }: _*))
            }
          });
          $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "contents", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus(new _root_.scala.xml.Text("\012\011"));
                $buf.$amp$plus({
                  {
                    new _root_.scala.xml.Elem(null, "version", _root_.scala.xml.Null, $scope, ({
                      val $buf = new _root_.scala.xml.NodeBuffer();
                      $buf.$amp$plus(new _root_.scala.xml.Text("1"));
                      $buf
                    }: _*))
                  }
                });
                $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
                $buf
              }: _*))
            }
          });
          $buf.$amp$plus(new _root_.scala.xml.Text("\012    "));
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "html", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "head", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus({
                  {
                    new _root_.scala.xml.Elem(null, "title", _root_.scala.xml.Null, $scope, ({
                      val $buf = new _root_.scala.xml.NodeBuffer();
                      $buf.$amp$plus(new _root_.scala.xml.Text("Scala"));
                      $buf
                    }: _*))
                  }
                });
                $buf
              }: _*))
            }
          });
          $buf.$amp$plus(new _root_.scala.xml.Text("\012      "));
          $buf.$amp$plus({
            {
              new _root_.scala.xml.Elem(null, "body", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus(xs);
                $buf
              }: _*))
            }
          });
          $buf.$amp$plus(new _root_.scala.xml.Text("\012    "));
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "code", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(new _root_.scala.xml.Text("\012    \012      class xyz[A] extends TypeConstraint\012      \012      def loopWhile[T](cond: =>Boolean)(body: =>(Unit @xyz[T])): Unit @ xyz[T] = {\012\011if (cond) {\012\011  body\012\011  loopWhile[T](cond)(body)\012\011}\012      }\012\012      def test() = {\012\011var x = 7\012\011loopWhile(x != 0) {\012\011  x = x - 1\012\011  (): @xyz[Int]\012\011}\012      }\012      \012    "));
          $buf
        }: _*))
      }
    }, {
      {
        var $md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
        $md = new _root_.scala.xml.UnprefixedAttribute("b", new _root_.scala.xml.Text("2"), $md);
        $md = new _root_.scala.xml.UnprefixedAttribute("d", new _root_.scala.xml.Text("4"), $md);
        $md = new _root_.scala.xml.UnprefixedAttribute("e", "5", $md);
        $md = new _root_.scala.xml.UnprefixedAttribute("a", new _root_.scala.xml.Text("1"), $md);
        $md = new _root_.scala.xml.UnprefixedAttribute("c", new _root_.scala.xml.Text("3"), $md);
        new _root_.scala.xml.Elem(null, "foo", $md, $scope)
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "entry", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(new _root_.scala.xml.Text("\012    "));
          $buf.$amp$plus("a,b,c".split(',').map(((item) => {
            {
              new _root_.scala.xml.Elem(null, "elem", _root_.scala.xml.Null, $scope, ({
                val $buf = new _root_.scala.xml.NodeBuffer();
                $buf.$amp$plus(item);
                $buf
              }: _*))
            }
          })));
          $buf.$amp$plus(new _root_.scala.xml.Text("\012    "));
          $buf
        }: _*))
      }
    }, {
      {
        var $md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
        $md = new _root_.scala.xml.UnprefixedAttribute("b", new _root_.scala.xml.Text("2"), $md);
        $md = new _root_.scala.xml.UnprefixedAttribute("a", new _root_.scala.xml.Text("1"), $md);
        new _root_.scala.xml.Elem(null, "k", $md, $scope)
      }
    }, {
      {
        var $md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
        $md = new _root_.scala.xml.UnprefixedAttribute("a", new _root_.scala.xml.Text("1"), $md);
        new _root_.scala.xml.Elem(null, "k", $md, $scope)
      }
    }, new _root_.scala.xml.Group({
      val $buf = new _root_.scala.xml.NodeBuffer();
      $buf.$amp$plus({
        {
          new _root_.scala.xml.Elem(null, "p", _root_.scala.xml.Null, $scope, ({
            val $buf = new _root_.scala.xml.NodeBuffer();
            $buf.$amp$plus({
              {
                new _root_.scala.xml.Elem(null, "lost", _root_.scala.xml.Null, $scope)
              }
            });
            $buf.$amp$plus({
              {
                new _root_.scala.xml.Elem(null, "t", _root_.scala.xml.Null, $scope, ({
                  val $buf = new _root_.scala.xml.NodeBuffer();
                  $buf.$amp$plus({
                    {
                      new _root_.scala.xml.Elem(null, "s", _root_.scala.xml.Null, $scope, ({
                        val $buf = new _root_.scala.xml.NodeBuffer();
                        $buf.$amp$plus({
                          {
                            new _root_.scala.xml.Elem(null, "r", _root_.scala.xml.Null, $scope)
                          }
                        });
                        $buf
                      }: _*))
                    }
                  });
                  $buf
                }: _*))
              }
            });
            $buf
          }: _*))
        }
      });
      $buf
    }), {
      {
        new _root_.scala.xml.Elem(null, "a", _root_.scala.xml.Null, $scope)
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "a", _root_.scala.xml.Null, $scope)
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "a", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(xml.NodeSeq.Empty);
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "a", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf
        }: _*))
      }
    }, {
      {
        new _root_.scala.xml.Elem(null, "a", _root_.scala.xml.Null, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(if (true)
            ""
          else
            "I like turtles");
          $buf
        }: _*))
      }
    }, {
      var $tmpscope: _root_.scala.xml.NamespaceBinding = $scope;
      $tmpscope = new _root_.scala.xml.NamespaceBinding("tns", "target1", $tmpscope);
      {
        val $scope: _root_.scala.xml.NamespaceBinding = $tmpscope;
        var $md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
        $md = new _root_.scala.xml.UnprefixedAttribute("name", x, $md);
        new _root_.scala.xml.Elem("wsdl", "definitions", $md, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(new _root_.scala.xml.Text("\012    "));
          $buf
        }: _*))
      }
    }, {
      var $tmpscope: _root_.scala.xml.NamespaceBinding = $scope;
      $tmpscope = new _root_.scala.xml.NamespaceBinding("tns", "target3", $tmpscope);
      {
        val $scope: _root_.scala.xml.NamespaceBinding = $tmpscope;
        var $md: _root_.scala.xml.MetaData = _root_.scala.xml.Null;
        $md = new _root_.scala.xml.UnprefixedAttribute("name", x, $md);
        new _root_.scala.xml.Elem("wsdl", "definitions", $md, $scope, ({
          val $buf = new _root_.scala.xml.NodeBuffer();
          $buf.$amp$plus(new _root_.scala.xml.Text("\012    "));
          $buf
        }: _*))
      }
    })

  for( (lit, orig) <- xmlLiterals zip originalXml ) {
    assert(lit xml_== orig)
  }
}
