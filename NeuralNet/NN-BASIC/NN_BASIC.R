
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
x<-cbind(c(0,0,1,1),c(1,1,0,0))
y<-cbind(c(0,0,1,1))




set.seed(120)
synapse_0<- matrix(rnorm(20), 2,1)


for(iter in 1:1000){
  
  #feed-forward
 
  
  
  print(synapse_0)
  
  layer_0<- x
  layer_1<- sigmoid(dotproduct(layer_0,synapse_0))
  
  print(layer_1)
  
  #change with respect to output
  layer_1_error<- layer_1-y
  
 # print(layer_1_error)
  #Multiply error with the first layer derivative 
  
  layer_1_delta<- layer_1_error*sigmoid_To_derivative(layer_1)
  
  synapse_0_derivative<- (t(layer_0) %*% layer_1_delta)
  
  synapse_0= synapse_0-synapse_0_derivative
  
  
}

print(layer_1)

