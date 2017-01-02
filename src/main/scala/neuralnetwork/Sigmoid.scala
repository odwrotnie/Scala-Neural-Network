package neuralnetwork

import scala.math.exp

object Sigmoid extends ActivationFunction {
	override def eval ( value: Double) : Double = {
		1d / (1d + exp(-value))
	}
}
