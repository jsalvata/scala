import scala.xml.ScalaXMLUnmarshaller

class Test {
  def f1 = {
    <?scala { ScalaXMLUnmarshaller }?><more><xml/></more>
    ;
    <xml/>
  }
  def f2= {
    <?scala { ScalaXMLUnmarshaller }?>
    ;
    <xml/>
  }
}
