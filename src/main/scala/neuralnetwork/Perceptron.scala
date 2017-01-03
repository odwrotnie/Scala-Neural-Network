package neuralnetwork

import scala.collection.mutable._
import scala.math._

object Perceptron {
  /**
    * @param layerNumbers first number is the number of inputs
    * @return
    */
  def apply(layerNumbers: Int*): Perceptron = {
    val layers: List[Layer] = layerNumbers.sliding(2).toList.map(l => Layer(l(0), l(1)))
    new Perceptron(layers = layers)
  }
}

class Perceptron(val layers: List[Layer]) {

  layers.foreach(_.perceptron = this)

  def run(inputs: Array[Double]) : Array[Double] = {
    require(inputs.length == layers.head.inputs)
    layers.foldLeft(inputs)((in, layer) => {
      layer.run(in)
      layer.neurons.map(_.output)
    })
  }

  def learn(inputs: Array[Double], outputs: Array[Double]) : Double = {

    val learnLevel : Double = 0.3
    val alfa : Double =  0.9

    for {
      layer <- layers.reverse
      neuron <- layer.neurons
    } {
      layer.prev match {
        case Some(previousLayer) =>
          val neuronError = neuron.error
          previousLayer.neurons foreach { prevNeuron =>
            neuron.pastErrors(prevNeuron.index) =
              learnLevel * neuronError * prevNeuron.output +
                alfa * neuron.pastErrors(prevNeuron.index)
            neuron.weights(prevNeuron.index) = neuron.pastErrors(prevNeuron.index) + neuron.weights(prevNeuron.index)
          }
          // Updates for the biais
          neuron.pastErrors(previousLayer.neurons.size) =
            -learnLevel * neuronError + alfa * neuron.pastErrors(previousLayer.neurons.size)
          neuron.weights(previousLayer.neurons.size) =
            neuron.pastErrors(previousLayer.neurons.size) + neuron.weights(previousLayer.neurons.size)
        case None =>
          val neuronError = neuron.error
          inputs.indices foreach { inputIndex =>
            neuron.pastErrors(inputIndex) =
              learnLevel * neuronError * inputs(inputIndex) +
                alfa * neuron.pastErrors(inputIndex)
            neuron.weights(inputIndex) = neuron.pastErrors(inputIndex) + neuron.weights(inputIndex)
          }
          // Updates for the biais
          neuron.pastErrors(inputs.size) =
            -learnLevel * neuronError + alfa * neuron.pastErrors(inputs.size)
          neuron.weights(inputs.size) = neuron.pastErrors(inputs.size) + neuron.weights(inputs.size)
      }
    }

    // Calculates the quadratic error between outputs and expected values
    val sum = outputs.zip(layers.last.neurons).map {
      case (output, neuron) => pow(output - neuron.output, 2)
    }.sum
    sqrt(sum)
  }

  class Synapses {
    val errors = collection.mutable.Map[(Neuron, Neuron), Double]()
    def setError(source: Neuron, destination: Neuron, value: Double): Unit = errors += (source, destination) -> value
    def getError(source: Neuron, destination: Neuron): Double = errors.getOrElse((source, destination), 0d)
  }

  def calculateErrors(inputs: Array[Double], outputs: Array[Double]): Unit = {

    //    val s = new Synapses

    for {
      layer <- layers.reverse
      neuron <- layer.neurons
    } {
      val nextOutput = layer.next match {
        case Some(nextLayer) =>
          nextLayer.neurons.zipWithIndex.map {
            case (nextNeuron, index) => nextNeuron.error * nextNeuron.weights(neuron.index)
          }.sum
        case None =>
          outputs(neuron.index) - neuron.output
      }
      neuron.error = neuron.output * (1 - neuron.output) * nextOutput
    }
  }

  override def toString: String = layers.zipWithIndex.map {
    case (layer, index) => s"Layer $index:\n$layer"
  } mkString("\n", "\n", "\n")
}
