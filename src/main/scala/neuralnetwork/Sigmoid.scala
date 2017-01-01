package neuralnetwork

import scala.math.exp

/**
  * Created by rzepaw on 01.01.2017.
  */
object Sigmoid extends ActivationFunction {
	override def eval ( value: Double) : Double = {
		1d / (1d + exp(-value))
	}
}
