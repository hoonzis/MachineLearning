module MachineLearning

type NeuralNetwork = {
    inputToHidden : float [,];
    hiddenToOutput : float [,];
    hidden : float[];
    output: float[];
}