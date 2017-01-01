package neuralnetwork

import scala.collection.mutable._
import scala.math.{pow, sqrt}
import scala.xml.{Elem, XML}

class Perceptron(val layers : Array[Layer]){

	def this(perceptronInputNum : Int, neuronLayerNum: Array[Int]) = {

		this(new Array[Layer](neuronLayerNum.size))

		//Inputs for first layer equals the inputs of the perceptron
		var layerInputNum = perceptronInputNum

		//Creates the layers with each the right number of neurons
		for(i <- 0 until neuronLayerNum.size){

			//creates the layer
			layers(i) = new Layer(layerInputNum, neuronLayerNum(i))

			//the number of inputs of the next
			//layer is the current number of neurons
			layerInputNum = neuronLayerNum(i)
		}
	}

	def run (inputs: Array[Double]) : Array[Double] = {

		//The inputs for the first layer are the perceptron inputs
		var layerInput = inputs

		for(i <- 0 until layers.size) {

			//Calculates the outputs of the layer
			layers(i).run(layerInput)

			//The outputs are the inputs for the next layer
			layerInput = layers(i).outputs
		}

		//returns the outputs of the last layer
		return layerInput
	}

	def learn (inputs: Array[Double], outputs: Array[Double]) : Double = {

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
				val neuronError = layer.errors(neuronIndex)

				//For every neuron in the previous layer
				for(previusNeuronIndex <- 0 until previousLayer.neurons.size){

					neuron.pastErrors(previusNeuronIndex) =
						learnLevel * neuronError * previousLayer.outputs(previusNeuronIndex) +
						alfa * neuron.pastErrors(previusNeuronIndex)

					neuron.weights(previusNeuronIndex) = neuron.pastErrors(previusNeuronIndex) + neuron.weights(previusNeuronIndex)
				}

				//updates for the biais
				neuron.pastErrors(previousLayer.neurons.size) =
					-learnLevel * neuronError + alfa * neuron.pastErrors(previousLayer.neurons.size)
				neuron.weights(previousLayer.neurons.size) = neuron.pastErrors(previousLayer.neurons.size) + neuron.weights(previousLayer.neurons.size)
			}
		}

		//For the first layer
		val layer = layers.head
		for(neuronIndex <- 0 until layer.neurons.size){

			//current neuron and error
			val neuron = layer.neurons(neuronIndex)
			val neuronError = layer.errors(neuronIndex)

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
			cumul += pow(outputs(i) - layers.last.outputs(i), 2)
		}
		sqrt(cumul)
	}

	def calculateErrors (inputs: Array[Double], outputs: Array[Double]) = {

		//For each layer from last to first
		for (layerIndex <- layers.size - 1 to(0,-1)) {
			//current layer
			val layer = layers(layerIndex)

			//For every neuron in the layer
			for(neuronIndex <- 0 until layer.neurons.size){

				//current neuron
				val neuron = layer.neurons(neuronIndex)
				//output for current neuron
				val neuronOutput = layer.outputs(neuronIndex)

				//Last factor of the error calculation
				if(layerIndex == layers.size - 1){ //Last layer
					layer.errors(neuronIndex) = neuronOutput * (1 - neuronOutput) * (outputs(neuronIndex) - neuronOutput)
				} else { //Hidden layers
					var tmp : Double = 0
					for(nextNeuronIndex <- 0 until layers(layerIndex + 1).neurons.size)
						tmp += layers(layerIndex + 1).errors(nextNeuronIndex) * layers(layerIndex + 1).neurons(nextNeuronIndex).weights(neuronIndex)

					layer.errors(neuronIndex) = neuronOutput * (1 - neuronOutput) * tmp
				}
				//Calculate the cumulated error of the layer
				//layer.cumulatedError =
				//	neuron.weights.reduceLeft( _ + layer.errors(neuronIndex) * _)
			}

        }
	}

	override def toString() : String = {
		var buf = new StringBuilder
		layers.foreach { layer => buf.append(layer.toString) }
		buf.toString
	}

	def toXml() : Elem = {

		val perceptronXml =
		<perceptron layers={layers.size.toString}>
			{for (layer <- layers) yield
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

		return perceptronXml
	}

	def saveXml(filePath : String) = {
		XML.save(filePath, toXml(), "UTF-8", true, null)
	}
}

object Perceptron {

	def loadXml(filePath : String) : Perceptron = {

		val perceptronXml = XML.load(filePath)
		val layers = new ListBuffer[Layer]

		for(layerXml <- perceptronXml \\ "layer") {
			val neurons = new ListBuffer[Neuron]
			val inputs : Int = (layerXml \ "@inputs").text.toInt

			for(neuronXml <- layerXml \\ "neuron") {

				val weights = new ListBuffer[Double]

				for(weightXml <- neuronXml \\ "weight") {
					weights += weightXml.text.toDouble
				}

				neurons += new Neuron(weights.toArray)
			}

			layers += new Layer(inputs, neurons.toArray)
		}

		new Perceptron(layers.toArray)
	}
}