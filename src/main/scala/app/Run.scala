package app

import neuralnetwork.Perceptron
import xml.{XMLDeserializeSupport, XMLSerializeSupport}

object Run
  extends App
  with XMLSerializeSupport
  with XMLDeserializeSupport {

  def generateXOR = {

    // We use a perceptron with 3 layers, the first with 5 neurons, the second with 10 and the las with only one
    val perceptron = Perceptron(2, 5, 10, 1)

    val inputs = Array[Array[Double]](
      Array[Double](0,0),
      Array[Double](0,1),
      Array[Double](1,0),
      Array[Double](1,1)
    )
    val outputs = Array[Double](0, 1, 1, 0)

    println("Training neural network with XOR")
    //100 iterations to train the network
    for(i <- 0 until 100) {
      for(i <- 0 until inputs.size) {
        perceptron.run(inputs(i))
        perceptron.calculateErrors(inputs(i), Array[Double](outputs(i)))
        perceptron.learn(inputs(i), Array[Double](outputs(i)))
      }
    }

    println("Saving the perceptron configuration in XOR.xml")
    saveXml("XOR.xml", perceptron)
  }

  def runXOR = {

    println("Loading the perceptron configuration from XOR.xml")
    val perceptron = loadPerceptron("XOR.xml")

    println("Evaluation of the inputs:")
    print("0,0 => ")
    perceptron.run(Array[Double](0, 0)).foreach(i =>{println(i)})

    print("0,1 => ")
    perceptron.run(Array[Double](0, 1)).foreach(i =>{println(i)})

    print("1,0 => ")
    perceptron.run(Array[Double](1, 0)).foreach(i =>{println(i)})

    print("1,1 => ")
    perceptron.run(Array[Double](1, 1)).foreach(i =>{println(i)})
  }

  generateXOR
  runXOR
}
