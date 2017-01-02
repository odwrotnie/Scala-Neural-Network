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

  val pastErrors = new Array[Double](weights.size)

  def run(inputs: Array[Double]) : Double = {

    //TODO: More functional style
    var cumul : Double = 0d
    for(i <- 0 until inputs.size){
      cumul += inputs(i) * weights(i)
    }
    eval(cumul)
  }

  override def toString() : String = {

    var buf = new StringBuilder("\n\t(")
    for(i <- weights.indices){
      buf.append("w")
      buf.append(i)
      buf.append(":")
      buf.append(weights(i))
      buf.append(", ")
    }
    return buf + ")"
  }
}
