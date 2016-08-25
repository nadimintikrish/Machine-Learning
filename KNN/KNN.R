#Import Data

data("iris")
View(iris)

#Train-test random data Split

i= sample(nrow(iris),nrow(iris)*0.75,rep=F)

x_learn = iris[,-5]
y_learn = iris[,5]

x_test = x_learn[-i,]
x_learn= x_learn[i,]
y_test = y_learn[-i]
y_learn= y_learn[i]

x_test[[4]]

#Final train and test data

learndata = cbind.data.frame(x_learn,y_learn)
testdata =  cbind.data.frame(x_test,y_test)


#Euclidean Distance

euclidDistance <- function(testdata,traindata,k=3){
  resultDist =0
  
    resultDist <- sqrt(sum((testdata-traindata)**2))
  
  return(resultDist)
}

#Algorithm Implementation

knnAlgo <- function(testdata,traindata,k=3){
  
  distances <- vector(mode = "numeric")
  
  for(group in 1:nrow(traindata) )
    {
      traindataVec <- traindata[group,1:ncol(traindata)-1]
      euclid_distance = euclidDistance(testdata,traindataVec)
      distances<-append(distances,euclid_distance)
    
    }
distanceMat <- cbind.data.frame(distances,traindata[[ncol(traindata)]])
#print(length(distances))
#print(length(traindata[[ncol(traindata)]]))
orderMat <- distanceMat[order(distances),]
resultClass <- names(sort(table(orderMat[1:k,2]),decreasing=TRUE)[1])

return(resultClass)
}

#Test Algorithm
truepositives =0
total = 0

for(rowtest in 1:nrow(testdata) )
{
  testVec <- testdata[rowtest,1:ncol(testdata)-1]
  resultclass <- knnAlgo(testVec,learndata,10)
  if(resultclass == testdata[rowtest,ncol(testdata)])
  {
    truepositives = truepositives+1
  }
  total= total+1
}

#Final Accuracy
cat("Accuracy is ",truepositives/total*100)
