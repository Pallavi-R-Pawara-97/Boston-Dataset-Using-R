# Load the data into R :

library(MASS)
data("Boston")
View(Boston)


# For data description :

? Boston

# We will split the data into training and testing sets :

set.seed(2)
#installed.packages("caTools")
library(caTools)  # sample.split function is present in this package
Split <- sample.split(Boston$medv , SplitRatio = 0.7)
Split

training_data <- subset(Boston , Split == "TRUE")
testing_data <- subset(Boston , Split == "FALSE")

# To view the correlation of variables :

plot(Boston$crim , Boston$medv , cex = 0.5 , xlab = "Crime rate" , ylab = "Price")

# Check correlation :

cr <- cor(Boston)
cr

# Creating scatterplot matrix :

attach(Boston)
library(lattice)
#? lattice
splom(~Boston[c(1:6,14)],groups = NULL , data = Boston, axis.line.tck = 0 , axis.text.alpha = 0)

splom(~Boston[c(7:14)], groups = NULL , data = Boston , axis.line.tck = 0 , axis.text.alpha = 0)

# Studying rm and medv :

plot(rm , medv)
abline(lm(medv~rm), col = "red") # Regression fit line

# We can use corrplot to visualise :

library(corrplot)

corrplot(cr , type = "lower")
corrplot(cr , method = "number")

# Finding Multicollinearity :
#installed.packages("caret")

library(caret)

# To exclude medv( output ) :
Boston_a = subset(Boston , select = -c(medv))
Boston_a

numericData <-  Boston_a[sapply(Boston_a, is.numeric)]
numericData

descrcor <- cor(numericData)
descrcor

#Varience Inflation Factor (vif) :

#installed.packages("car")

library(car)

model <- lm(medv~., data = training_data)
model

vif(model)

# Now to create the model we will use all columns :

model <- lm(medv~. , data = training_data)
model

# For description of the model :
summary(model)

# Model creation after removing zn :

model <- lm(medv~ crim + indus + chas + nox + rm + age + dis +tax + rad + ptratio + black + lstat , data = training_data )
model
summary(model)

# Model after removing indus and age :
model <- lm(medv~ crim + chas + nox + rm + dis +tax + rad + ptratio + black + lstat , data = training_data )
model
summary(model)

# Now we can use this model to predict the output of test set :

predic <- predict(model , testing_data)
predic

# Compare Actual and Predicted :

actual_predicted <- data.frame(testing_data$medv , predic)
View(actual_predicted)

# To compare predicted values and actual values , we can use plots :

plot(testing_data$medv , type = "l" , lty = 1.8 , col = "green")
lines(predic , type =  "l" , lty = 1.8 , col = "blue")

# Accuracy :
error <- testing_data$medv - predic
#error

MAPE <- mean(abs (error /testing_data$medv))*100
MAPE

MSE <- mean(error*error)
MSE

RMSE <- sqrt(MSE)
RMSE

MAE <- mean(abs(error))
MAE

mean(error) # this is equal to zero

#hist(error  ,  breaks=20)
hist(error)

summary(predic)
summary(model)
.............END.........................................