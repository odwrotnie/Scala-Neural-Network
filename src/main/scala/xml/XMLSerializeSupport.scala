package xml

import java.io.FileOutputStream
import java.nio.channels.Channels

import neuralnetwork._

import scala.io.Codec
import scala.xml._

trait XMLSerializeSupport {

  def codec = Codec.UTF8

  def toXml(n: Neuron): Elem =
    <neuron index={ n.index.toString } inputs={n.weights.size.toString}>
      { n.weights.map(w => <weight>{ w.toString }</weight>) }
    </neuron>

  def toXml(l: Layer): Elem =
    <layer inputs={ l.inputs.toString } neurons={ l.neurons.length.toString }>
      { l.neurons.map(toXml) }
    </layer>

  def toXml(p: Perceptron): Elem =
    <perceptron layers={ p.layers.size.toString }>
      { p.layers.map(toXml) }
    </perceptron>

  def saveXml(filePath: String, perceptron: Perceptron): Unit = {
    val xml = toXml(perceptron)
    // XML.save(filePath, xml, codec.name, xmlDecl = true, null)
    save(xml, filePath)
  }

  private def save(node: Node, fileName: String) = {
    val pp = new PrettyPrinter(80, 2)
    val fos = new FileOutputStream(fileName)
    val writer = Channels.newWriter(fos.getChannel(), codec.name)
    try {
      writer.write(s"<?xml version='1.0' encoding='${ codec.name }'?>\n")
      writer.write(pp.format(node))
    } finally {
      writer.close()
    }
    fileName
  }
}
