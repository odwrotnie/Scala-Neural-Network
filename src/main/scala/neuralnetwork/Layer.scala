package neuralnetwork

object Layer {
  def apply(inputs: Int, neurons: Int): Layer = {
    // The last neuron is for bias (+1)
    new Layer(inputs, Array.fill(neurons)(Neuron.create(inputs + 1)))
  }
}

class Layer(val inputs: Int, val neurons: Array[Neuron]) {

  neurons.foreach(_.layer = this)
  var perceptron: Perceptron = _
  lazy val index: Int = perceptron.layers.indexOf(this)
  lazy val prev: Option[Layer] = if (index > 0) Some(perceptron.layers(index - 1)) else None
  lazy val next: Option[Layer] = if (index + 1 < perceptron.layers.size) Some(perceptron.layers(index + 1)) else None
  lazy val isHidden: Boolean = next.isDefined

  def run(inputs: Array[Double]): Unit = {
    require(inputs.length == this.inputs)
    neurons foreach { neuron =>
      neuron.output = neuron.run(inputs)
    }
  }

  override def toString: String = neurons.mkString("[\n - ", "\n - ", "\n]")
}
