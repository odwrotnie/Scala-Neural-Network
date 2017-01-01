package neuralnetwork

class Layer(val inputNum: Int, val neurons : Array[Neuron]) {

	val outputs = new Array[Double](neurons.size)
	val errors = new Array[Double](neurons.size)
	var cumulatedError: Double = _

	def this(inputNum: Int, neuronNum: Int) = {
		//The last neuron inputs are used for the biais
		this(inputNum, Array.fill(neuronNum)(new Neuron(inputNum + 1)))
	}

	def run (inputs: Array[Double]) = {
		for(i <- 0 until outputs.size) {
			outputs(i) = neurons(i).run(inputs)
		}
	}

	override def toString() : String = {
		var buf = new StringBuilder("[")
		neurons.foreach { neuron =>
			buf.append(neuron)
			buf.append(",")
		}
		buf.append("\n]\n")
		buf.toString
	}
}
