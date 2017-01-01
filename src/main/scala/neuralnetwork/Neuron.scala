package neuralnetwork

import scala.math.random

class Neuron (val weights: Array[Double]){

  import Sigmoid.eval

  val pastErrors = new Array[Double](weights.size)

  def this (inputNum: Int) = {

    //Use [-2.4, 2.4] as initial weight range
    this(Array.fill(inputNum)(random * (2.4 *2)- 2.4))
  }

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