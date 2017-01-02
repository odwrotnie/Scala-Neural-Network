package xml

import neuralnetwork._

import scala.io.Codec
import scala.xml._

trait XMLSerializeSupport {

  def codec = Codec.UTF8

  def toXml(x: Any): Elem = x match {
    case n: Neuron =>
      <neuron inputs={n.weights.size.toString}>{ n.weights.map(w => <weight>{ w.toString }</weight>) }</neuron>
    case l: Layer =>
      <layer inputs={ l.inputs.toString } neurons={ l.neurons.size.toString }>{ l.neurons.map(toXml) }</layer>
    case p: Perceptron =>
      <perceptron layers={ p.layers.size.toString }>{ p.layers.map(toXml) }</perceptron>
  }

  def saveXml(filePath: String, perceptron: Perceptron) = {
    XML.save(filePath, toXml(perceptron), codec.name, true, null)
  }
}
