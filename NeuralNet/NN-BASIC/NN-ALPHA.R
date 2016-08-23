
alphas<- 0.1
sigmoid<- function(x){
  
  output<- 1/(1+exp(-x))
  
  return (output)
}

sigmoid_To_derivative<- function(output){
  
  return (output*(1-output))
  
}

dotproduct<- function (x,y){
  
  return (x %*% y)
}
#x<-cbind(c(0,0,1,1),c(0,1,0,1),c(0,1,0,1))
#y<-cbind(c(0,1,1,0))
x<- finalmodel
y<- outputMat


set.seed(1)
synapse_0<- matrix(rnorm(960), 8,12)
synapse_1<- matrix(rnorm(960), 12,4)

for(iter in 1:60000){
  
  #feed-forward
  
  
  
  #print(synapse_0)
  
  layer_0<- x
  layer_1<- sigmoid(dotproduct(layer_0,synapse_0))
  layer_2<- sigmoid(dotproduct(layer_1,synapse_1))
  #print(layer_1)
  
  #change with respect to output
  layer_2_error<- layer_2-y
  
  layer_2_delta<- layer_2_error*sigmoid_To_derivative(layer_2)
  
  # print(layer_1_error)
  #Multiply error with the first layer derivative 
  
  layer_1_error<- (layer_2_delta %*% t(synapse_1))
  layer_1_delta<- layer_1_error*sigmoid_To_derivative(layer_1)
  
  synapse_1_derivative<- (t(layer_1) %*% layer_2_delta)
  synapse_0_derivative<- (t(layer_0) %*% layer_1_delta)
  
  synapse_1= synapse_1-(alphas*synapse_1_derivative)
  synapse_0= synapse_0-(alphas*synapse_0_derivative)
  
  
}

print(synapse_0)

print(round(layer_2))

