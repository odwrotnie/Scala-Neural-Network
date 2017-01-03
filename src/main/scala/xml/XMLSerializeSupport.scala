package xml

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

  def saveXml(filePath: String, perceptron: Perceptron): Unit =
    XML.save(filePath, toXml(perceptron), codec.name, xmlDecl = true, null)
}
