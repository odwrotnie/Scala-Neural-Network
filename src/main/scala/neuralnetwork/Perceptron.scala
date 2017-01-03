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

    //For all the layers but the first
    for(layerIndex <- layers.size - 1 to(1, -1)) {

      //current layer
      val layer = layers(layerIndex)
      //previous layer
      val previousLayer = layers(layerIndex - 1)

      //For every neuron in the current layer
      for(neuronIndex <- 0 until layer.neurons.size){

        //current neuron and error
        val neuron = layer.neurons(neuronIndex)
        val neuronError = neuron.error

        //For every neuron in the previous layer
        for(previusNeuronIndex <- 0 until previousLayer.neurons.size){

          neuron.pastErrors(previusNeuronIndex) =
            learnLevel * neuronError * previousLayer.neurons(previusNeuronIndex).output +
              alfa * neuron.pastErrors(previusNeuronIndex)

          neuron.weights(previusNeuronIndex) = neuron.pastErrors(previusNeuronIndex) + neuron.weights(previusNeuronIndex)
        }

        //updates for the biais
        neuron.pastErrors(previousLayer.neurons.size) =
          -learnLevel * neuronError + alfa * neuron.pastErrors(previousLayer.neurons.size)
        neuron.weights(previousLayer.neurons.size) =
          neuron.pastErrors(previousLayer.neurons.size) + neuron.weights(previousLayer.neurons.size)
      }
    }

    //For the first layer
    val layer = layers.head
    for(neuronIndex <- 0 until layer.neurons.size){

      //current neuron and error
      val neuron = layer.neurons(neuronIndex)
      val neuronError = neuron.error

      //For every input
      for(inputIndex <- 0 until inputs.size){

        neuron.pastErrors(inputIndex) =
          learnLevel * neuronError * inputs(inputIndex) +
            alfa * neuron.pastErrors(inputIndex)

        neuron.weights(inputIndex) = neuron.pastErrors(inputIndex) + neuron.weights(inputIndex)
      }

      //updates for the biais
      neuron.pastErrors(inputs.size) =
        -learnLevel * neuronError + alfa * neuron.pastErrors(inputs.size)
      neuron.weights(inputs.size) = neuron.pastErrors(inputs.size) + neuron.weights(inputs.size)
    }

    //Calculates the cuadratic error between outputs and expected values
    //TODO: Use a functional style
    var cumul : Double = 0
    for (i <- 0 until outputs.size) {
      cumul += pow(outputs(i) - layers.last.neurons(i).output, 2)
    }
    sqrt(cumul)
  }

  def calculateErrors(inputs: Array[Double], outputs: Array[Double]): Unit = {


    // Hidden layers

    for {
      layer <- layers.reverse
      neuron <- layer.neurons
    } {
      if (layer.isHidden) {
        val nextLayer = layer.next.get
        val tmp = nextLayer.neurons.zipWithIndex.map {
          case (nextNeuron, index) => nextLayer.neurons(index).error * nextNeuron.weights(neuron.index)
        }.sum
        neuron.error = neuron.output * (1 - neuron.output) * tmp
      } else {
        neuron.error = neuron.output * (1 - neuron.output) * (outputs(neuron.index) - neuron.output)
      }
    }
  }

  override def toString: String = layers.zipWithIndex.map {
    case (layer, index) => s"Layer $index:\n$layer"
  } mkString("\n", "\n", "\n")
}
