# Reading csv file
data <- read.csv('Student.csv',header=TRUE)
print(head(data))
#Data preprocessing
print(dim(data))
Scores = data$Scores
print(Scores)
summary(data)
#sampling data
ind <- sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))
traindata = data[ind==1,]
testdata = data[ind==2,]
print(traindata)
print(testdata)
#model building
model = lm(Scores~Hours,data=traindata)
print(model)
summary(model)
error = sigma(model)/mean(traindata$Scores)
print(error*100)
plot(data$Hours,data$Scores,col="red")
abline(model)
#Prediction
predicted = predict(model,newdata = testdata)
print(predicted)
hour = data.frame(Hours=4.2)
score = predict(model,newdata = hour)
print(score)

