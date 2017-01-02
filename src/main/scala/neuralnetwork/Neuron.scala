package neuralnetwork

import scala.math.random

object Neuron {

  val WEIGHT_RANGE = 2.4

  def create(inputs: Int): Neuron = {
    Neuron(Array.fill(inputs)(random * (WEIGHT_RANGE * 2) - WEIGHT_RANGE))
  }
}

case class Neuron(weights: Array[Double]) {

  import Sigmoid.eval

  val pastErrors = new Array[Double](weights.length)

  def run(inputs: Array[Double]): Double = {
    val cumul: Double = inputs.zip(weights).map(l => l._1 * l._2).sum
    eval(cumul)
  }

  override def toString: String = weights.map(d => f"$d%.3f").mkString("N(", ", ", ")")
}
