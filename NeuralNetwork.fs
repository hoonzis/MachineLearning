module MachineLearning.NeuralNetwork

type NeuralNetwork = {
    inputToHidden : float [,]
    hiddenToOutput : float [,]
    hidden : float[]
    output: float[]
    input: float[]
}