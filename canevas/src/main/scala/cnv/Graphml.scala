package cnv

import java.io._

object Graphml {

  val HEAD_TEXT: String = 
  """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
  |<graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:java="http://www.yworks.com/xml/yfiles-common/1.0/java" xmlns:sys="http://www.yworks.com/xml/yfiles-common/markup/primitives/2.0" xmlns:x="http://www.yworks.com/xml/yfiles-common/markup/2.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:y="http://www.yworks.com/xml/graphml" xmlns:yed="http://www.yworks.com/xml/yed/3" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://www.yworks.com/xml/schema/graphml/1.1/ygraphml.xsd">
  |  <!--Created by yEd 3.19.1.1-->
  |  <key attr.name="Description" attr.type="string" for="graph" id="d0"/>
  |  <key for="port" id="d1" yfiles.type="portgraphics"/>
  |  <key for="port" id="d2" yfiles.type="portgeometry"/>
  |  <key for="port" id="d3" yfiles.type="portuserdata"/>
  |  <key attr.name="url" attr.type="string" for="node" id="d4"/>
  |  <key attr.name="description" attr.type="string" for="node" id="d5"/>
  |  <key for="node" id="d6" yfiles.type="nodegraphics"/>
  |  <key for="graphml" id="d7" yfiles.type="resources"/>
  |  <key attr.name="url" attr.type="string" for="edge" id="d8"/>
  |  <key attr.name="description" attr.type="string" for="edge" id="d9"/>
  |  <key for="edge" id="d10" yfiles.type="edgegraphics"/>
  |  <graph edgedefault="directed" id="G">
  |    <data key="d0"/>""".stripMargin

  val TAIL_TEXT: String = """
  |  </graph>
  |  <data key="d7">
  |    <y:Resources/>
  |  </data>
  |</graphml>""".stripMargin

  val A_COLOR = "#CCFFCC"
  val C_COLOR = "#99CCFF"
  val G_COLOR = "#FFCC99"
  val T_COLOR = "#FF99CC"
  val DEFAULT_COLOR = "#FFFFFF"

  val COLOR_TABLE: Map[Char, String] = Map('A' -> A_COLOR, 'C' -> C_COLOR, 'G' -> G_COLOR, 'T' -> T_COLOR).withDefaultValue(DEFAULT_COLOR)

  def coloredNode(base: Char, n: Int, reverse: Boolean = false) = {
    """"
    |   <node id="n""".stripMargin + n.toString + """">
    |     <data key="d5"/>
    |     <data key="d6">
    |       <y:ShapeNode>
    |         <y:Geometry height="30.0" width="30.0" x="""".stripMargin + n*30 + """.0" y="0.0"/>
    |         <y:Fill color="""".stripMargin + COLOR_TABLE.getOrElse(base, DEFAULT_COLOR) + """" transparent="false"/>
    |         <y:BorderStyle color="#000000" raised="false" type="line" width="1.0"/>
    |         <y:NodeLabel alignment="center" autoSizePolicy="content" fontFamily="Dialog" fontSize="12" fontStyle="plain" hasBackgroundColor="false" hasLineColor="false" height="17.96875" horizontalTextPosition="center" iconTextGap="4" modelName="custom" textColor="#000000" verticalTextPosition="bottom" visible="true" width="11.330078125" x="9.3349609375" xml:space="preserve" y="6.015625">""".stripMargin + base + """<y:LabelModel><y:SmartNodeLabelModel distance="4.0"/></y:LabelModel><y:ModelParameter><y:SmartNodeLabelModelParameter labelRatioX="0.0" labelRatioY="0.0" nodeRatioX="0.0" nodeRatioY="0.0" offsetX="0.0" offsetY="0.0" upX="0.0" upY="-1.0"/></y:ModelParameter></y:NodeLabel>
    |         <y:Shape type="fatarrow""".stripMargin + (if (reverse) {"2"} else {""}) + """"/>
    |       </y:ShapeNode>
    |     </data>
    |   </node>"""".stripMargin
  }

  def generate_graphml_file_from_sequence(sequence: String, file: String, reverse: Boolean = false) = {
    val bw = new BufferedWriter(new FileWriter(new File(file)))
    bw.write(HEAD_TEXT)

    val seq = {
      if (reverse) {
        sequence.reverse
      } else {
        sequence
      }
    }

    seq.zipWithIndex foreach {
      case (c, i) => {
        bw.write(coloredNode(c, i, reverse))
      }
    }

    bw.write(TAIL_TEXT)
    bw.close()
  }

}