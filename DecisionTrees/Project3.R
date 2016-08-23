
#dataTraining <- read.csv("C:/Krishna/ML-622/Project3/contact-lenses.data", sep="")
dataTraining <- read.csv("C:/Krishna/ML-678/Project3/fishing.data", sep="")
uniqueOracle<- unique(dataTraining$Oracle)
  
Entropy<-function()
{
 
distCountVec<-vector(mode = "numeric")
probdist<-vector(mode = "numeric")

for(j in 1:length(uniqueOracle))
{
  sumDistinct=0
  for(i in 1:nrow(dataTraining)) 
  {
    
    if(dataTraining$Oracle[i]==uniqueOracle[j])
    
      sumDistinct= sumDistinct+1
  }
  distCount=sumDistinct
  

distCountVec<-append(distCountVec,distCount)
probdist<- append(probdist,(distCount/nrow(dataTraining)))

}

  
  
  probVec<- sapply(probdist, function(x) (x)*log2(x))
  sum <- sum(probVec)
  
    return(-sum)
}

 

getDataSet<- function(i,j,dataTraining){
  
  dataTraining[dataTraining[j] == toString(unique(dataTraining[[j]])[i]) ,]
  
}



atomicEntropy<- function(atomicData,entropySum){
  
  dataSetProb<-apply(atomicData,2,function(x) x/sum(x))
  
  dataSetlog<-apply(dataSetProb,2,function(x) (x)*log2(x))
  dataSetlog<- replace(dataSetlog,is.nan(dataSetlog),0)
  dataSetEntropy<- apply(dataSetlog,2,function(x) -sum(x))
 
  return(dataSetEntropy)
}


EntropyS<- Entropy()

infoGainVector<- vector(mode = "numeric")
  
for(i in 2:(ncol(dataTraining)-1)){
  
  nodedataSetTemp<-table(dataTraining$Oracle,dataTraining[[i]]) 
  nodemargintemp<- margin.table(nodedataSetTemp, 2)
  entropySum<- vector(mode = "numeric")
  nodedataSetEntropy<-atomicEntropy(nodedataSetTemp)
  nodedataSetEntropy<-replace(nodedataSetEntropy,is.nan(nodedataSetEntropy),0)
  
  sum=0
  for(i in 1:length(nodedataSetEntropy)){
    sum= sum+ nodedataSetEntropy[i]*(nodemargintemp[i]/sum(nodemargintemp))
  }
  
 nodeGainInfo<- EntropyS-sum
 #print(GainInfo)
 infoGainVector<- append(infoGainVector,nodeGainInfo)
 
  
}

NodeColumn<- which.max(infoGainVector)+1

nodeTabletemp<- table(dataTraining$Oracle,dataTraining[[NodeColumn]])
nodeSetEntropy<-atomicEntropy(nodeTabletemp)
nodeSetEntropy<-replace(nodeSetEntropy,is.nan(nodeSetEntropy),0)

dataTraining[[1]]<-NULL
NodeColumn<- NodeColumn-1

AlgorithmDecision<- function(node,newdataTraining,nodeSetEntropy){
  
  if(ncol(newdataTraining)<=1){
    print(unique(newdataTraining$Oracle))
  }
  else
    {
  for(i in 1:length(unique(newdataTraining[[node]]))){
    #nodeEntropyIndex<-which(names(nodeSetEntropy)== toString(unique(newdataTraining[[node]])[i]))
    #nodeEntropy<- nodeSetEntropy[nodeEntropyIndex]
    nodeEntropy<-EntropyS
    truncData<-getDataSet(i,node,newdataTraining)
    print(names(truncData)[node])
    print(unique(truncData[[node]]))
    truncData[[node]]<-NULL
    newDatatrain<-truncData
    #print(newDatatrain)
    
    if(length(unique(newDatatrain$Oracle))<=1){
     print(unique(newDatatrain$Oracle))
      return
    }
   else{ 
     print("Next Node is........")
    GainVector<- vector(mode = "numeric")
    for(i in 1:(ncol(newDatatrain)-1)){
      
      dataSetTemp<-table(newDatatrain$Oracle,newDatatrain[[i]]) 
      margintemp<- margin.table(dataSetTemp, 2)
      entropySum<- vector(mode = "numeric")
      dataSetEntropy<-atomicEntropy(dataSetTemp)
      dataSetEntropy<-replace(dataSetEntropy,is.nan(dataSetEntropy),0)
      sum=0
      for(i in 1:length(dataSetEntropy)){
        sum= sum+ dataSetEntropy[i]*(margintemp[i]/sum(margintemp))
      }
      
      GainInfo<- nodeEntropy-sum
      GainVector<- append(GainVector,GainInfo)
     # print(GainVector)
      
    }
        #print(GainVector)
        if((length(GainVector)==0)||(length(GainVector)==1)){
          print(unique(newDatatrain[[1]]))
           print(unique(newDatatrain$Oracle))
          #print()
        return
    }
    else{
    nodeElement<- which.max(GainVector)
    nodeTabletemp<- table(newDatatrain$Oracle,newDatatrain[[i]])
    nodeSetEntropy<-atomicEntropy(nodeTabletemp)
    nodeSetEntropy<-replace(nodeSetEntropy,is.nan(nodeSetEntropy),0)
   # print(nodeSetEntropy)
    AlgorithmDecision(nodeElement,newDatatrain,nodeSetEntropy)
    }
   }
  }
} 
}


AlgorithmDecision(NodeColumn,dataTraining,nodeSetEntropy)

library(data.tree)

ForeCast<- Node$new("Forecast")
Sunny<-ForeCast$AddChild("Sunny")
Cloudy<- ForeCast$AddChild("Cloudy")
Rainy<-ForeCast$AddChild("Rainy")
SunnyWind<-Sunny$AddChild("Wind")
SunnyWeakWind<- SunnyWind$AddChild("Weak")
SunnystrongWind<- SunnyWind$AddChild("Strong")
SunnyWeakWater<- SunnyWeakWind$AddChild("Water")
SunnyWeakWater$AddChild("Warm")$AddChild("No")
SunnyWeakWater$AddChild("Cold")$AddChild("No")
SunnyWeakWater$AddChild("Moderate")$AddChild("Yes")
Cloudy$AddChild("Yes")
RainyAir<- Rainy$AddChild("Air")
RainyAirCool<- RainyAir$AddChild("Cool")
RainyAirCool$AddChild("No")
RainyAirWarm<- RainyAir$AddChild("Warm")
RainyAirWind<- RainyAirWarm$AddChild("Wind")
RainyAirWind$AddChild("Strong")$AddChild("Yes")
RainyAirWind$AddChild("Weak")$AddChild("No")
print(ForeCast)
plot(ForeCast)

TearRate<- Node$new("TearRate")
Reduced<- TearRate$AddChild("Reduced")
Normal<- TearRate$AddChild("Normal")
Reduced$AddChild("None")
NormAst<-Normal$AddChild("Astigmatism")
NormAstno<- NormAst$AddChild("No")
NormAstyes<- NormAst$AddChild("Yes")
NormAstnoAge<- NormAstno$AddChild("Age")
noAgeYng<- NormAstnoAge$AddChild("Young")$AddChild("Soft")
noAgepre<- NormAstnoAge$AddChild("Pre-Presbyopic")$AddChild("Soft")
noAgepres<- NormAstnoAge$AddChild("Presbyopic")
noAgepresSight<- noAgepres$AddChild("Prescription")
noAgepresSight$AddChild("Myope")$AddChild("None")
noAgepresSight$AddChild("HyperMetrope")$AddChild("Soft")
YesPres<- NormAstyes$AddChild("Prescription")
YesPresMyope<- YesPres$AddChild("Myope")$AddChild("Hard")
YespresHyper<- YesPres$AddChild("HyperMyope")
YesAge<- YespresHyper$AddChild("Age")
YesAge$AddChild("Young")$AddChild("Hard")
YesAge$AddChild("Pre-Presbyopic")$AddChild("None")
YesAge$AddChild("Presbyopic")$AddChild("None")
plot(TearRate)
