package app

import neuralnetwork.Perceptron
import xml.{XMLDeserializeSupport, XMLSerializeSupport}

object Run
  extends App
    with XMLSerializeSupport
    with XMLDeserializeSupport {

  val FILE_NAME = "XOR.xml"
  val TRAIN_ITERATIONS = 10000

  val inputs = Array[Array[Double]](
    Array[Double](0,0),
    Array[Double](0,1),
    Array[Double](1,0),
    Array[Double](1,1)
  )
  val outputs = Array[Double](0, 1, 1, 0)

  def generateXOR = {

    // We use a perceptron with 3 layers, the first with 5 neurons, the second with 10 and the las with only one
    val perceptron = Perceptron(2, 5, 10, 1)

    println(s"Perceptron: $perceptron")

    println("Training neural network with XOR")
    //100 iterations to train the network
    for {
      _ <- 0 until TRAIN_ITERATIONS
      (input, output) <- inputs.zip(outputs)
    } {
      perceptron.run(input)
      perceptron.calculateErrors(input, Array[Double](output))
      perceptron.learn(input, Array[Double](output))
    }

    println(s"Perceptron: $perceptron")

    println(s"Saving the perceptron configuration in $FILE_NAME")
    saveXml(FILE_NAME, perceptron)
  }

  def runXOR = {

    println(s"Loading the perceptron configuration from $FILE_NAME")
    val perceptron = loadPerceptron(FILE_NAME)

    inputs foreach { input =>
      val i = input.mkString(", ")
      val o = perceptron.run(input).mkString(", ")
      println(s"$i => $o")
    }
  }

  generateXOR
  runXOR
}
