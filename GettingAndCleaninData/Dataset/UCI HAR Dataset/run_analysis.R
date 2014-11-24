library(reshape)
library(plyr)
counttest <- c(1)
counttrain <- c(1)
testfiles <- vector()
trainfiles <- vector()
testData <- data.frame()
trainData <- data.frame()
mergedData <- data.frame()
varNames <- vector()
testfiles <- list.files('test',full.names = T)[-1]
testfiles <- c(testfiles[2],testfiles[1],testfiles[3])
trainfiles <- list.files('train',full.names = T)[-1]
trainfiles <- c(trainfiles[2],trainfiles[1],trainfiles[3])
varNames <- read.table("features.txt",as.is = T)[[2]]
varNames <- gsub("\\(\\)","",varNames)
varNames <- gsub("-","",varNames)
varNames <- c(varNames,"Subject","Activity")
for(i in testfiles){
        fileData <- read.table(i,header = T,as.is = T)
        ifelse(length(counttest)==1,{testData = rbind(testData,fileData)}, {testData = cbind(testData,fileData)})
        counttest <- c(counttest, 1)
}
for(i in trainfiles){
        fileData <- read.table(i,header = T,as.is = T)
        ifelse(length(counttrain)==1,{trainData = rbind(trainData,fileData)},{trainData = cbind(trainData,fileData)})
        counttrain <- c(counttrain,1)
}
names(testData) <- varNames
names(trainData) <- varNames
mergedData <- rbind(trainData,testData)
meanvar <- grep("*mean*",varNames)
stdvar <- grep("*std*",varNames)
mergedData <- mergedData[,c(meanvar,stdvar,length(varNames)-1,length(varNames))]
mergedData$Activity <- factor(mergedData$Activity,labels = c('WALKING','WALKING_UPSTAIRS','WALKING_DOWNSTAIRS','SITTING','STANDING','LAYING'))
moltenData <- melt(mergedData, id.vars = c("Activity", "Subject"))
castData <- cast(Subject + variable ~ Activity, data = moltenData, fun = mean)
write.table(x = castData,"castData.txt",row.names = F)