package neuralnetwork

object Layer {
  def apply(inputs: Int, neurons: Int): Layer = {
    // The last neuron is for bias (+1)
    new Layer(inputs, Array.fill(neurons)(Neuron.create(inputs + 1)))
  }
}

class Layer(val inputs: Int, val neurons: Array[Neuron]) {

	val outputs = new Array[Double](neurons.size)
	val errors = new Array[Double](neurons.size)
	var cumulatedError: Double = _

	def run(inputs: Array[Double]) = {
    require(inputs.length == this.inputs)
		for(i <- 0 until outputs.size) {
			outputs(i) = neurons(i).run(inputs)
		}
	}

	override def toString() : String = neurons.mkString("[", ", ", "]")
}
