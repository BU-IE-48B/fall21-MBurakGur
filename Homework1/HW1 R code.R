library(data.table)
library(ggplot2)
library(scatterplot3d)

current_folder <- getwd()

datasetX <- 'uWaveGestureLibrary_X_TRAIN'
datasetY <- 'uWaveGestureLibrary_Y_TRAIN'
datasetZ <- 'uWaveGestureLibrary_Z_TRAIN'


train_data_pathX=sprintf('uWaveGestureLibrary_X_TRAIN',current_folder,datasetX,datasetX)
train_data_pathY=sprintf('uWaveGestureLibrary_Y_TRAIN',current_folder,datasetY,datasetY)
train_data_pathZ=sprintf('uWaveGestureLibrary_Z_TRAIN',current_folder,datasetZ,datasetZ)

train_dataX=fread(train_data_pathX)
train_dataY=fread(train_data_pathY)
train_dataZ=fread(train_data_pathZ)

train_dataX
train_dataY
train_dataZ


setnames(train_dataX,'V1','class')
setnames(train_dataY,'V1','class')
setnames(train_dataZ,'V1','class')

train_dataX=train_dataX[order(class)]
train_dataY=train_dataY[order(class)]
train_dataZ=train_dataZ[order(class)]




train_dataX[, names(train_dataX[,-1]) := Reduce(`+`, train_dataX[,-1], accumulate = TRUE)]
train_dataY[, names(train_dataY[,-1]) := Reduce(`+`, train_dataY[,-1], accumulate = TRUE)]
train_dataZ[, names(train_dataZ[,-1]) := Reduce(`+`, train_dataZ[,-1], accumulate = TRUE)]


train_dataX[,class:=as.character(class)]
train_dataX[,id:=1:.N]
long_trainX=melt(train_dataX,id.vars=c('id','class'))
setnames(long_trainX, 'value', 'valueX')

train_dataY[,class:=as.character(class)]
train_dataY[,id:=1:.N]
long_trainY=melt(train_dataY,id.vars=c('id','class'))
setnames(long_trainY, 'value', 'valueY')

train_dataZ[,class:=as.character(class)]
train_dataZ[,id:=1:.N]
long_trainZ=melt(train_dataZ,id.vars=c('id','class'))
setnames(long_trainZ, 'value', 'valueZ')

whole_data <- cbind(long_trainX, long_trainY[,'valueY'])
whole_data <- cbind(whole_data, long_trainZ[,'valueZ'])


whole_data[,time:=as.numeric(gsub("\\D", "", variable))-1]

head(whole_data)


# remove variable
whole_data=whole_data[,list(id,class,time,valueX,valueY,valueZ)]
whole_data=whole_data[order(id,time)]

class1 = whole_data[class== 1 & id==1, ]
class2 = whole_data[class== 2 & id==131, ]
class3 = whole_data[class== 3 & id==270, ]
class4 = whole_data[class== 4 & id==414, ]
class5 = whole_data[class== 5 & id==509, ]
class6 = whole_data[class== 6 & id==614, ]
class7 = whole_data[class== 7 & id==695, ]
class8 = whole_data[class== 8 & id==836, ]

gesture = rbind(class1,class2,class3,class4,class5,class6,class7,class8)
#Graph 3D

scatterplot3d(x=class1$valueX, y=class1$valueY, z=class1$valueZ )

#boxplot

ggplot(gesture, aes(x=valueX, y=valueY, color=class)) +
  geom_boxplot()

#long_train[,interval_id:=NULL]
whole_data[,interval_id:=cut(time,2, ordered_result=T),by=list(id)]
str(whole_data)
whole_data[,interval_id:=as.numeric(interval_id)]

#long_train[id==1]
stats=whole_data[,list(mean=mean(valueX),median=median(valueX),stdev=sd(valueX)),by=list(class,id,interval_id)]
head(stats)

#
interval_stats=dcast(stats,id+class~paste0('int_',interval_id),value.var='mean')
head(interval_stats)


ggplot(data=interval_stats, aes(x=id, y=int_1,fill=class)) +
  geom_bar(stat="identity")


