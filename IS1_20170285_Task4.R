library(rpart)
library(rpart.plot)
library(tree)
#read from csv
data<-read.csv("E:/FCI/year 4/second term/ERP/task4/car-dataset.csv")
#show  first 6 rows
head(data)
# show num. of Row and column
dim(data)
# Create train/test set
set.seed(0)
dt = sort(sample(nrow(data), nrow(data)*.8))
train<-data[dt,]
test<-data[-dt,]
#show num. of Row and column in each set
dim(train)
dim(test)
# build tree by training
fit <- rpart(Label~., data =train,
             control=rpart.control(minsplit=1),
             parms=list(split='information'),
             method = 'class')
# predict test
predict_test<-predict(fit, test, type = 'class')
# showing the correctness of prediction in table ,the diagonal :is a correct prediction ,row acual data ,column prediction
table_mat <- table(test$Label, predict_test)
table_mat    
# print the accuracy using previous table 
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
print(paste('Accuracy for test', accuracy_Test))

#the accuracy is high =962% which mean 96% of test set is correctly predict 
