package xml

import neuralnetwork._

import scala.collection.mutable.ListBuffer
import scala.xml._

trait XMLDeserializeSupport {

  def loadPerceptron(filePath : String): Perceptron = {

    val perceptronXml = XML.load(filePath)
    val layers = new ListBuffer[Layer]

    for(layerXml <- perceptronXml \\ "layer") {
      val neurons = new ListBuffer[Neuron]
      val inputs : Int = (layerXml \ "@inputs").text.toInt
      for(neuronXml <- layerXml \\ "neuron") {
        val index: Int = (neuronXml \ "@index").text.toInt
        val weights = new ListBuffer[Double]
        for(weightXml <- neuronXml \\ "weight") {
          weights += weightXml.text.toDouble
        }
        neurons += new Neuron(weights.toArray)
      }
      layers += new Layer(inputs, neurons.toArray)
    }

    new Perceptron(layers.toList)
  }
}
