
dataTraining <- read.csv("C:/Krishna/ML-622/Project4/gameData.csv")
#dataTraining <- read.csv("C:/Krishna/ML-622/Project4/fishingData.csv")

##Read the input dataframe to model matrix
inputMatrixModel<- function(dataTraining){
  
tempmat<-dataTraining[[1]]
matvec1<- model.matrix(~tempmat) 
if(is.numeric(tempmat)){ matvec1<-matvec1[,-1]}
for(i in 2:ncol(dataTraining)-1){
  newtemp<- dataTraining[[i]]
  modelmat<- model.matrix(~newtemp)
  if(is.numeric(newtemp))
    modelmat<- modelmat[,-1]
  matvec1<-cbind( matvec1,modelmat)
  
}


if(is.numeric(dataTraining[[1]])){matvec1<-matvec1[,-1]}
if(!is.numeric(dataTraining[[1]])) {matvec1<-matvec1[,-c(1:length(unique(dataTraining[[1]])))]}

return(matvec1)
}
##Modeled Output
output<- dataTraining[[ncol(dataTraining)]]
outputMat<- model.matrix(~output)


alphas<- c(0.1,0.2,0.5,0.7,1)

hiddensize<- 15

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

testingData<- function(testData,synapse_t0,synapse_t1){
  
  layer_t0<- testData
  layer_t1<- sigmoid(dotproduct(layer_t0,synapse_t0))
  layer_t2<- sigmoid(dotproduct(layer_t1,synapse_t1)) 
  return(round(layer_t2))
}
#x<-cbind(c(0,0,1,1),c(0,1,0,1),c(0,1,0,1))
#y<-cbind(c(0,1,1,0))
x<- inputMatrixModel(dataTraining)
y<- outputMat

x<- cbind(x,c(rep(1,nrow(x)))) #Adding Bias Vector

set.seed(1)
synapse_0<- matrix(rnorm(ncol(x)*hiddensize*100), ncol(x),hiddensize)
synapse_1<- matrix(rnorm(ncol(x)*hiddensize*100), hiddensize,ncol(y))
plotx<- vector(mode="numeric")
ploty<-vector(mode="numeric")
for (alpha in alphas)
{
for(iter in 1:60000){
  
  #feed-forward

  layer_0<- x
  layer_1<- sigmoid(dotproduct(layer_0,synapse_0))
  layer_2<- sigmoid(dotproduct(layer_1,synapse_1))
  
  #change with respect to output
  layer_2_error<- layer_2-y
  
  layer_2_delta<- layer_2_error*sigmoid_To_derivative(layer_2)
  
  # print(layer_1_error)
  #Multiply error with the first layer derivative 
  
  layer_1_error<- (layer_2_delta %*% t(synapse_1))
  layer_1_delta<- layer_1_error*sigmoid_To_derivative(layer_1)
  
  synapse_1_derivative<- (t(layer_1) %*% layer_2_delta)
  synapse_0_derivative<- (t(layer_0) %*% layer_1_delta)
  
  synapse_1= synapse_1-(alpha*synapse_1_derivative)
  synapse_0= synapse_0-(alpha*synapse_0_derivative)
  
  if(iter%%3000==0){
    error<- mean(abs(layer_2_error))
    plotx<-append(plotx,error)
    ploty<- append(ploty,iter)
  }
  
}
plot(ploty,plotx)

print(round(layer_2))

plotx<- vector(mode="numeric")
ploty<-vector(mode="numeric")

}
## Output For Fishing
## Test Data- Wind,Water,Air,Forecast,Fish
##Strong,Cold,Warm,Sunny

testmodel<- c(1,0,1,0,0,1,1,1,0,1,1)

testingData(t(testmodel),synapse_0,synapse_1)

##Output for GameData

testdataNew<- read.csv("C:/Krishna/ML-622/Project4/gameDataTrain.csv")
testdataNew<-data.matrix(testdataNew, rownames.force = NA)
testingData(testdataNew,synapse_0,synapse_1)
