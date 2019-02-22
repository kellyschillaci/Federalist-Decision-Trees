setwd("C:\\Users\\kdoyl\\OneDrive\\Documents\\IST 707")
papers1 <- read.csv("fedPapers85 (1).csv")


no_disp1<-papers1[papers1$author!='dispt',]
no_disp1<-no_disp1[no_disp1$author!='Jay',]
no_disp1<-no_disp1[no_disp$author!='HM',]
final <- no_disp1[complete.cases(no_disp1),]
#create subset with just disputes
disp1<-papers[papers$author=='dispt',]

#create random index to generate train and testing data
randl1<-sample(1:dim(final)[1])
cutpoint2_31 <- floor(2*dim(final)[1]/3)
#create train data and test data
trainData1 <- final[randl1[1:cutpoint2_31],]
testData1 <- final[randl1[(cutpoint2_31+1):dim(final)[1]],]
#remove filename column
trainData1<-trainData1[,-2]
testData1<-testData1[,-2]
#change data from numberic to factor
trainData=trainData1 %>% mutate_if(is.numeric, funs(as.factor))
testData=testData1 %>% mutate_if(is.numeric, funs(as.factor))

dt1=J48(author~., data = trainData1)
summary(dt)
e1 <- evaluate_Weka_classifier(dt1, numFolds = 10, seed = 100, class = TRUE)
e1

dt21=J48(author~., data = trainData1, control=Weka_control(U=FALSE,M=10,
                                                         C=0.05))
summary(dt21)

e21<- evaluate_Weka_classifier(dt21, numFolds = 30, seed = 1, class = TRUE)
e21

pred1<-predict(d21, newdata = testData1, type = c("class"))
comp1<-data.frame(testData1[,1],pred1)
colnames(comp1) <- c("test","pred")
comp1$match<-ifelse(comp1$test==comp1$pred,1,0)
match_percent1<-(sum(comp1$match)/nrow(comp1))*100
paste('Testing data correct percentage',match_percent1)


disp_pred1<-predict(dt21, newdata = disp1, type = c("class"))
disp_result1<-data.frame(disp1[,2],disp_pred1)
colnames(disp_result1)<-c('paper_name','predict')
knitr::kable(disp_result1)

fit1 <- rpart(author ~ ., data = trainData1, method = "class", control = rpart.control(minsplit = 1, cp = 0.2))
summary(fit1)
predicted=predict(fit1,testData1, type="class")
(head(predicted,n=10))
#(head(test, n=10))
plot(fit1)
text(fit1)
fancyRpartPlot(fit1)
