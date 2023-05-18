library(glmnet)
library(dplyr)
library(caret)
library(mlbench)
library(pROC)
library(correlation)
library(corrplot)
library(plotmo)

setwd('E:/R-Programming-Practices/Data Visualization/LASSO Regression')
data<- read.csv('GSE150910_combined.csv')
data<- data[,-c(1,3,4,5,6)]
data[data=='-Inf']<- 7.5

# Inspect the data
sample_n(data, 3)

# Split the data into training and test set
set.seed(123)
training.samples <- data$Sample %>% 
  createDataPartition(p = 0.7, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

# Assign predictor and response variables
x <- model.matrix(Sample~., train.data)[,-1]
# Convert the outcome (class) to a numerical variable
y <- ifelse(train.data$Sample == "Disease", 1, 0)

# Find the best lambda using cross-validation
glmnet(x, y, family = "binomial", alpha = 1, lambda = NULL)

set.seed(123) 
cv.lasso <- cv.glmnet(x, y, alpha = 1, family = "binomial")
cv.lasso$lambda.min

coef(cv.lasso)
plot(cv.lasso)
plot(cv.lasso$glmnet.fit, label = FALSE, xvar ='lambda')
plot_glmnet(cv.lasso$glmnet.fit)
plot(roc.glmnet(cv.lasso, 
                newx = x, 
                newy = y ))      

###############################################################################
# Fit the final model on the TRAINING DATA
model <- glmnet(x, y, alpha = 1, family = "binomial",
                lambda = cv.lasso$lambda.min)
# Display regression coefficients
coef(model)

## Make predictions on the TEST DATA
x.test <- model.matrix(Sample ~., test.data)[,-1]
probabilities <- model %>% predict(newx = x.test)
predicted.classes <- ifelse(probabilities > 0.5, "Disease", "Control")

## Asses Model accuracy
observed.classes <- test.data$Sample
mean(predicted.classes == observed.classes)
###############################################################################

## Alternative way
correlations <- cor(data[,-1])   #correlations among all the predictors

jpeg('Correlation.jpeg', height = 12, width = 10, units = 'cm', res = 600)
corrplot(correlations, method = 'color', tl.cex = 0.35, tl.col = 'black',
         order = 'hclust')  
dev.off()
summary(glm(Sample == "Disease" ~ SAMD11 , 
            family=binomial, 
            data=data ))
summary(glm(Sample == "Disease" ~ .,        #the dot includes all the variables 
            family=binomial,
            data=data))  

X <-  model.matrix(Sample ~., 
                   data=data)

Y <- data[,"Sample"]=="Disease"

cv.model<- cv.glmnet(x=X,y=Y, 
                     family = "binomial",
                     alpha=1)                        #alpha=1 is lasso

l.min<- cv.model$lambda.min

cv.model
coef(cv.model)
plot(cv.model)

plot(cv.model$glmnet.fit, label = F, xvar = 'lambda')
plot_glmnet(cv.model$glmnet.fit)

plot(roc.glmnet(cv.model, 
                newx = X, 
                newy = Y ))
#Final model
lasso.model <- glmnet(x=X,y=Y, 
                      family = "binomial", 
                      alpha=1, 
                      lambda = l.min)
lasso.model$beta
plot(lasso.model$beta)

plot(lasso.model)