doMC::registerDoMC(parallel::detectCores())

nestedCV<-function(xlist,y,nfold=10){
  tpb<-txtProgressBar(0,nfold)
  testLabels<-matrix(NA,nrow(xlist[[1]]),length(xlist))
  colnames(testLabels)<-names(xlist)
  folds=sample(1:nfold,nrow(xlist[[1]]), replace=T)
  for(x in 1:nfold){
    for(f in 1:length(xlist)) {
      textData<-as.matrix(xlist[[f]])
      trainX<-textData[-which(folds==x),]
      testX<-textData[which(folds==x),]
      trainModel<-glmnet::cv.glmnet(x=trainX, y=y[-which(folds==x)],nfold=10, 
                                    parallel=T)
      testLabels[which(folds==x),f]<-predict(trainModel,newx=testX,
                                             s="lambda.min", type="response")
      
    }
    setTxtProgressBar(tpb,x)
  }
  return(testLabels)
}
