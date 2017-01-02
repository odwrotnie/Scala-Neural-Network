package neuralnetwork

object Layer {
  def apply(inputs: Int, neurons: Int): Layer = {
    // The last neuron is for bias (+1)
    new Layer(inputs, Array.fill(neurons)(Neuron.create(inputs + 1)))
  }
}

class Layer(val inputs: Int, val neurons: Array[Neuron]) {

	val outputs = new Array[Double](neurons.length)
	val errors = new Array[Double](neurons.length)
	def cumulatedError: Double = errors.sum

	def run(inputs: Array[Double]): Unit = {
    require(inputs.length == this.inputs)
    outputs.indices foreach { i =>
			outputs(i) = neurons(i).run(inputs)
		}
	}

	override def toString: String = neurons.mkString("[\n - ", "\n - ", "\n]")
}
