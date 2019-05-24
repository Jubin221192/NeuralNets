data_set <- read.csv(file.choose())
data <- data_set
View(head(data_set))
str(data)

#There is no NA values in the data-set
any(is.na(data))

#When we will train a neural network it is a good practice to normalize the data 
#For this first we will scale the data using min and max

#here 2 mins we want to apply this ovcer the columns
maxs<- apply(data,2,max)
maxs

mins <- apply(data,2,min)
mins

# Will use in scale function ( a generic function which will retuen a numeric matrix)
# Here I am getting the data and subtracting each data point from the minimum value 
# of the column and then divided by the maximum value - the minimum value.

sacled_neural_data <- scale(data,center=mins, scale = maxs - mins)
View(sacled_neural_data)

#As it a matrix I need to convert it into a data frame
sacaled_neural <- as.data.frame(sacled_neural_data)
View(sacaled_neural)

#Splitting the data into train and test
library(caTools)
set.seed(101)


split_data <- sample.split(data$Class, SplitRatio = 0.70)

train_data <- subset(data split = TRUE)
test_data <- subset(data, split == FALSE)

str(train_data)

#Building the neural nets
install.packages("neuralnet")

library(neuralnet)

# Here we are doing a classifiation problem thus, we wil set linear.output = FALSE
# And we are selecting the first hidden layer of 10 neurons

nural_model<- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy, data = train_data
                        ,hidden = 9, linear.output = FALSE)
plot(nural_model)

#requires numeric/complex matrix/vector arguments

# Neural net is bit of a black box and it is hard to interpret
# Thus we can't say much about the fittings, weights and even the model
# But we can say that this training algorithm is converged and it is ready to be tested on some test data set

# The black lines shows the connection between each layer and weights on each connection and
# the blue lines represent the bias term which is added in aech step
data<- read.csv(file.choose())

library(caTools)
set.seed(101)
split = sample.split(data$Class, SplitRatio = 0.70)

train_data = subset(data, split = TRUE)
test_data = subset(data, split = FALSE)

str(train_data)


library(neuralnet)

neural_mod <- neuralnet(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train_data,hidden=10,
                        linear.output=FALSE)
plot(neural_mod)

#Prediction
predicted_values <- compute(neural_mod,test[,1:4])

#Here we only need the net result
str(predicted_values)

#This is basically the probabilities either belonging to zero class and one class
head(predicted_values$net.result)

# thus I round off the probabilities using the sapply functions
final_predictions <- sapply(predicted_values$net.result, round)
head(final_predictions)

#creating a confusion matrix

table(final_predictions, test_data$Class)

#final_predictions   0   1
 #               0 229   0
 #              1   0 183


#checking with other model; Here I am using random forest
#First I am factoring the class column

data$Class <- factor(data$Class)
set.seed(101)

split <- sample.split(data$Class, SplitRatio = 0.70)

train_ran <- subset(data, split= TRUE)
test_ran <- subset(data, split = FALSE)

#fitting a random forest model
install.packages("randomForest")
library(randomForest)
rand_model <- randomForest(Class ~ ., data = train_ran)
rand_pred <- predict(rand_model, test_ran)  
table(rand_pred, test_ran$Class)

#final_predictions   0   1
 #               0 762  1
 #              1   2 610
  
  