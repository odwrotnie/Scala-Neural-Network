package xml

import neuralnetwork.Perceptron

import scala.io.Codec
import scala.xml._

trait XMLSerializeSupport {

  def codec = Codec.UTF8

  def toXml(x: Any): Elem = x match {
    case p: Perceptron =>
      <perceptron layers={ p.layers.size.toString }>
        {for (layer <- p.layers) yield
        <layer inputs={layer.inputNum.toString} neurons={layer.neurons.size.toString}>
          {for (neuron <- layer.neurons) yield
          <neuron inputs={neuron.weights.size.toString}>
            {for (weight <- neuron.weights) yield
            <weight>{weight.toString}</weight>
            }
          </neuron>
          }
        </layer>
        }
      </perceptron>
  }

  def saveXml(filePath: String, perceptron: Perceptron) = {
    XML.save(filePath, toXml(perceptron), codec.name, true, null)
  }
}
