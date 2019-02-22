setwd("C:\\Users\\kdoyl\\OneDrive\\Documents\\IST 707")

library(tm); library(ggplot2); library(wordcloud); library(lda); library(reshape2)
NovelsCorpus <- Corpus(DirSource("fedPapers"))
(getTransformations())
(ndocs<-length(NovelsCorpus))

(summary(NovelsCorpus))


(minTermFreq <- ndocs * 0.0001)
(maxTermFreq <- ndocs * 1)



Novels_dtm <- DocumentTermMatrix(NovelsCorpus,
                                 control = list(
                                   stopwords = TRUE,
                                   wordLengths=c(3, 15),
                                   removePunctuation = T,
                                   removeNumbers = T,
                                   tolower=T,
                                   stemming = T,
                                   remove_separators = T,
                                   stopwords = MyStopwords,
                                   bounds = list(global = c(minTermFreq, maxTermFreq))
                                 ))
inspect(Novels_dtm)
DTM_mat <- as.matrix(Novels_dtm)
Novels_M_N1 <- apply(DTM_mat, 1, function(i) round(i/sum(i),3))
Novels_M_N1[1:5,1:5]
Novels_Matrix_Norm <- t(Novels_M_N1)
Novels_Matrix_Norm[1:5,1:5]

str(DTM_mat)
(DTM_mat[,1])

disp_test <- as.matrix(Novels_Matrix_Norm[1:11,])
str(disp_test)
dt<- data.frame(as.matrix(disp_test))
dt <- tibble::rownames_to_column(dt, "document")
dt[1:5,1:5]

hamilton_df <- as.matrix(Novels_Matrix_Norm[12:63,])
str(hamilton_train)
hamilton_df<- data.frame(as.matrix(hamilton_df))
hamilton_df <- tibble::rownames_to_column(hamilton_df, "document")
hamilton_df$Author <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
hamilton_df[1:5,1:5]

(head(sort(as.matrix(hamilton_df)[13,], decreasing = TRUE), n=20))



madison_df <- as.matrix(Novels_Matrix_Norm[71:85,])
madison_df<- data.frame(as.matrix(madison_df))
madison_df <- tibble::rownames_to_column(madison_df, "document")
madison_df$Author <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
madison_df[1:5,1:5]

(head(sort(as.matrix(madison_df)[13,], decreasing = TRUE), n=20))

train <- as.data.frame(rbind(cbind(hamilton_df), cbind(madison_df)))

# ## Randomly sample cases to create independent training and test data
Partition <- createDataPartition(y = train$Author,p = 0.5, list = FALSE)
Training = train[Partition,] # Create the training sample
dim(Training)
Test = train[-Partition,] # Create the test sample
dim(Test)



library(dplyr)

Training[1:20,1:20]
Training$Author




Test[1:13,1:20]
Test$Author

## install.packages("rpart")
## install.packages('rattle')
## install.packages('rpart.plot')
## install.packages('RColorBrewer')
## install.packages("Cairo")
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(Cairo)
Training$Author=factor(Training$Author)
Test$Author=factor(Test$Author)

Training <- Training[(-c(1))]
Test <- Test[(-c(1))]
colnames(Training)



fit <- rpart(Author ~ ., data = Training, method = "class", control = rpart.control(minsplit = 1, cp = 0.2))
summary(fit)
predicted=predict(fit,Test, type="class")
(head(predicted,n=10))
#(head(test, n=10))
plot(fit)
text(fit)
fancyRpartPlot(fit)

install.packages("tree")
library(tree)
tree(Author ~., data=Training)

library("RWeka")
m=J48(Author~., data = Training, control=Weka_control(U=FALSE, M=2, C=0.1))
e <- evaluate_Weka_classifier(m, numFolds = 10, seed = 1, class = TRUE)
pred=predict (m, newdata = test, type = c("class"))



