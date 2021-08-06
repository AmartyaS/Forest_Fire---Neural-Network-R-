#INSTALLING THE REQUIRED PACKAGES
install.packages("neuralnet")
library(neuralnet)

#LOADING UP THE DATA AND MODIFYING IT 
data <- read.csv(file.choose())
View(data)
str(data)
#ONE HOT EN-CODING OF CATEGORICAL VALUES
data$size_category <- ifelse(data$size_category=="small",0,1)

#NORMALIZATION OF THE DATA TABLE
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
data_n <- data[,3:31]
data_n <- as.data.frame(lapply(data_n, normalize))
attach(data)

#DATA SAMPLING
ind <- sample(2,nrow(data_n),replace=T,prob=c(0.8,0.2))
train <- data_n[ind==1,]
test <- data_n[ind==2,]
View(train)

#FORMATION OF NEURAL NETWORK
model <- neuralnet(size_category~.,data = train,hidden = c(2,2),algorithm = 'backprop',learningrate = 0.001,stepmax = 1e+08,linear.output = F,act.fct = 'tanh')
plot(model)

#RESULT CALCULATION
result <- compute(model,test[1:28])
result
pred1 <-result$net.result
pred1

#CHECKING FOR ACCURACY
cor(pred1,test$size_category) #91.47% ACCURACY