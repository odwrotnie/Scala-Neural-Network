package neuralnetwork

object Layer {
  def apply(inputs: Int, neurons: Int): Layer = {
    // The last neuron is for bias (+1)
    new Layer(inputs, (0 until neurons).map(i => Neuron.create(i, inputs + 1)).toArray)
  }
}

class Layer(val inputs: Int, val neurons: Array[Neuron]) {

	def run(inputs: Array[Double]): Unit = {
    require(inputs.length == this.inputs)
    neurons foreach { neuron =>
			neuron.output = neuron.run(inputs)
		}
	}

	override def toString: String = neurons.mkString("[\n - ", "\n - ", "\n]")
}
