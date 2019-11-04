

##2b)
data  <- read.csv("/Users/chanukya/Documents/GitHub/DataMining/HW2/leaf/leaf.csv")
#View(data)
#Aspect ratio , Elongation and Solidity
#changing column names
column_names <- c("Class","SpecimenNumber","Eccentricity","AspectRatio","Elongation","Solidity","StochasticConvexity","IsoperimetricFactor","MaximalIndentationDepth","Lobedness","AvgIntensity","AvgContrast","Smoothness","ThirdMoment","Uniformity","Entropy")
for(i in 1:length(column_names)){
  names(data)[i]<- column_names[i]
}
colnames(data)

problem_a_columns <- c("AspectRatio","Elongation","Solidity")
for(i in 1:length(problem_a_columns)){
  boxplot(data[problem_a_columns[i]], main =  problem_a_columns[i])
}

#b)
data <- data.frame(scale(data))
#par( mfrow = c( 1, 3 ) )
for(i in 1:length(problem_a_columns)){
  boxplot(data[problem_a_columns[i]], main =  problem_a_columns[i])
}

#We see we have outliers in AspectRatio and Solidity


 
# c) #using Z distribution to identify the errors
data1 <- data

boxplot <- boxplot(data[,"AspectRatio"], data1[,"Elongation"], data1[,"Solidity"], names=c('Aspect Ratio','Elongation','Solidity'), main="Box Plot for attributes Aspect Ratio, Elongation, and Solidity")

outliers_aspect_ratio <- boxplot(data1[,"AspectRatio"])$out
outliers_elongation <- boxplot(data1[,"Elongation"])$out
outliers_solidity <- boxplot(data1[,"Solidity"])$out

x=boxplot.stats(data1[,"AspectRatio"])$out # The outliers are displayed in $out value:
x # outliers in aspect ratio
y=boxplot.stats(data1[, "Elongation"])$out # There are 0 outliers in $out.
y # outliers in elongation
z=boxplot.stats(data1[,"Solidity"])$out # The outliers are displayed in $out value:
z # ouliers in solidity

index_of_aspect <- !data1[,4]%in%boxplot.stats(data1[,4])$out
index_of_elongation <- !data1[,5]%in%boxplot.stats(data1[,5])$out
index_of_solidity <- !data1[,6]%in%boxplot.stats(data1[,6])$out

nooutlier_data1 <- data1[index_of_aspect & index_of_elongation & index_of_solidity,]
nooutlier_data1[,1] <- as.factor(nooutlier_data1$Class)
nooutlier_data1$ID <- 1:nrow(nooutlier_data1)

boxplot_values <- boxplot(data1[,"AspectRatio"], data1[,"Elongation"], data1[,"Solidity"], names=c('Aspect Ratio','Elongation','Solidity'), main="Box Plot for attributes Aspect Ratio, Elongation, and Solidity")



# data1 <- data
# outliers_aspect <- boxplot(data1$AspectRatio, plot = FALSE)$out
# data1 <- data1[which(data1$AspectRatio %in% outliers_aspect),]
# boxplot(data1$AspectRatio)
# # 
# outliers_elongation <- boxplot(data1$Elongation, plot = FALSE)$out
# #data1 <- data1[which(data1$Elongation %in% outliers_elongation),]
#  boxplot(data1$Elongation)
# 
# POPULATION PARAMETER CALCULATIONS
# aspectratio <- aspectratio - mean(aspectratio)/sd(aspectratio)
# aspect <- aspectratio[aspectratio > 3]
# aspect
# #find index of outliers
# index_outliers_aspect <- match(aspect, aspectratio)
# print(nrow(data1))
# #removing row of outliers
# data1 <- data
# for (i in 1:length(index_outliers_aspect)){
#   data1 <- data1[-c(i),]
# }
# #print(nrow(data1))
# boxplot(data1$AspectRatio)
# 
# 
# solidity <- data1$Solidity
# hist(solidity) #histogram
# 
# #POPULATION PARAMETER CALCULATIONS
# solidity <- (solidity - mean(solidity))/sd(solidity)
# solidity_ratio <- solidity[solidity > 3]
# solidity_ratio
# #no outliers
# boxplot(data1$Solidity)



#d)
data1 <- data
in_train <- sample(nrow(data1))
data1 <- data1[in_train,]
train_data <- data1[1:300,]
test_data  <- data1[301:nrow(data1),]
library(C50)
train_data$Class <- as.factor(train_data$Class)
vars = c("SpecimenNumber","Eccentricity","AspectRatio","Elongation","Solidity","StochasticConvexity","IsoperimetricFactor","MaximalIndentationDepth","Lobedness","AvgIntensity","AvgContrast","Smoothness","ThirdMoment","Uniformity","Entropy")
tree_mod <- C5.0(x = train_data[, vars], y = train_data$Class)
summary(tree_mod)
plot(tree_mod)
#cross  Validation
#cross validation of data is uncessary in Randoforest as the data and variables are already implicity partioned. 
predicted <- predict(tree_mod, newdata = test_data[, vars])
test_data$Class <- as.factor(test_data$Class)
cm <- confusionMatrix(test_data$Class,predicted)
accuracy_rpart <- cm$overall["Accuracy"]
test_Dat <- test_data$Class
count = 0
for (i in 1:length(test_Dat)){
  if (predicted[i] == test_Dat[i]){
    count = count+1
  }
}
error_rate_C5.0 = 1- (count/length(test_Dat))
error_rate_C5.0




#e)
set.seed(12345)

# Set up caret to perform 10-fold cross validation repeated 3 times

in_train <- sample(nrow(data1))
data1 <- data1[in_train,]
train_data <- data1[1:300,]
test_data  <- data1[301:nrow(data1),]
train_data$Class <- as.factor(train_data$Class)
vars = c("SpecimenNumber","Eccentricity","AspectRatio","Elongation","Solidity","StochasticConvexity","IsoperimetricFactor","MaximalIndentationDepth","Lobedness","AvgIntensity","AvgContrast","Smoothness","ThirdMoment","Uniformity","Entropy")
#colnames(train_data)
#crossvalidation
caret.control <- trainControl(method = "repeatedcv",
                              number = 3,
                              repeats = 2)
rpart.cv <- train(Class ~ .,
                  data = train_data,
                  method = "rpart",
                  trControl = caret.control,
                  tuneLength = 15)
summary(rpart.cv)
plot(rpart.cv$finalModel)

predicted <- predict(rpart.cv, newdata = test_data[, vars])
predicted
test_data$Class <- as.factor(test_data$Class)
cm <- confusionMatrix(test_data$Class,predicted)
accuracy_rpart <- cm$overall["Accuracy"]
test_Dat <- test_data$Class
count = 0
for (i in 1:length(test_Dat)){
  if (predicted[i] == test_Dat[i]){
    count = count+1
  }
}
error_rate_CART = 1- (count/length(test_Dat))
error_rate_CART

accuracy_rpart
e1<-0.6410256
e2<-0.4871795 

n<-length(data1)
d<-e1-e2
sigma1sq= e1*(1-e1)/n
sigma2sq<-e2*(1-e2)/n
sigmatsq<-sqrt(sigma1sq+sigma2sq)

Z<-2.33 #confidence level 98%
dt1<-d+(Z*sigmatsq)
dt2<-d-(Z*sigmatsq)
dt<-c(dt2,dt1)
print(dt)
#from the results we can say they are statistically insignificant


test1 <- function(a,b){
  predicted <- predict(a,b)
  return(predicted)
}

#input<- data.frame(19,0.451,2.700,0.5169,0.6800,0.8421,0.609,0.02530,0.1100,0.007984,0.042948,0.0050183,0.00071981,2.7289e-05,0.42553)
input <- test_data[1, vars]
predicted <- test1(tree_mod,input)
predicted
