install.packages('dplyr')
library(dplyr)



##1

# a) Yes
# It is required to use data mining practices to forecast the future values based on previous data,it is not something which is database and retrieving the information.

#b)No
#since they vary drastically and they don't depend on the previous data.

#c) Yes
# suppose we are able to derive relationship if particular students gets "A"  in one subject then there is high probability on of getting "A" in operating system they we can predict the students who are likely to "A" in operating systems.

#d)Yes
# we do it by association method which is part data mining process

#d)No
# we are not forecasting anything, we are just retrieving the data which is present. we can do it by simple SQL query.

##2)
#    Index               Discrete or Continous      quantitative or qualitative       nominal or ordinal or interval or ration
#a) Cellphone brands          Discrete                      qualitative                             nominal
#b) IQ levels                 continous                      quantitative                            ratio
#c) The price of laptios      Continous                      quantitative                           Interval
#d) Pass or Fail              Discrete                        qualitative                           nominal                     



##3)
#simple matching distance
sm_distance <- function(s1,s2){
count = 0
for ( i in 1:length(s1)){
  if(s1[i]==s2[i]){
    count = count+1
  }}
return ((count)/(length(s1)+length(s2)))
}
x = c(1,0,1,1,0,1,0) 
y = c(1,1,0,1,0,0,1)
sm_distance(x,y) 
#print(output) 




#Jaccard Coefficient
j_distance <- function(s1,s2){
  count = 0
  for ( i in 1:length(s1)){
    count <- count + s1[i]*s2[i]
  }
  return ((count)/length(s1))
}
x = c(1,0,1,1,0,1,0) 
y = c(1,1,0,1,0,0,1)
j_distance(x,y) 
#print(output) 



#cosine_Similarity
Cosine_similarity <- function(s1,s2){
  count = 0
  for ( i in 1:length(s1)){
    count = count+ s1[i]*s2[i]
  }
  newData1<- sapply(s1, function(x) x^2)
  newData1 <- sqrt(sum(newData1))
  newData2<- sapply(s2, function(x) x^2)
  newData2 <- sqrt(sum(newData2))
  return ((count)/(newData1*newData2))
}
x = c(1,0,1,1,0,1,0) 
y = c(1,1,0,1,0,0,1)
Cosine_similarity(x,y)

x = c(1,0,1,1,0,1,0) 
newData2<- sapply(x, function(y) y-mean(x))
newData2


#correlation

Correlation <- function(s1,s2){
newData1<- sapply(s1, function(y) y-mean(x))
newData1_d <- sapply(newData1, function(x) x^2)
newData1 <- sum(newData2)
newData2<- sapply(s2, function(y) y-mean(x))
newData2_d <- sapply(newData2, function(x) x^2)
newData2 <- sum(newData2)
return(newData1*newData2)/(sqrt(sum(newData1_d))* sqrt(sum(newData2_d)))
}
x = c(1,0,1,1,0,1,0) 
y = c(1,1,0,1,0,0,1)
Correlation(x,y)


#Hamming distance
Hamming_distance <- function(s1,s2){
  count = 0
  for ( i in 1:length(s1)){
    count = count+ s1[i]*s2[i]
  }
  newData1<- sapply(s1, function(x) x^2)
  newData1 <- sqrt(sum(newData1))
  newData2<- sapply(s2, function(x) x^2)
  newData2 <- sqrt(sum(newData2))
  return ((count)/(newData1*newData2))
}
x = c(1,0,1,1,0,1,0) 
y = c(1,1,0,1,0,0,1)
Cosine_similarity(x,y)

x = c(1,0,1,1,0,1,0) 
newData2<- sapply(x, function(y) y-mean(x))
newData2



#hamming distance
Hamming_distance <- function(s1,s2){
  count = 0
  for ( i in 1:length(s1)){
    if (s1[i] != s2[i]){
    count = count+ sum(s1[i]+s2[i])
    }}
  return (count)
}
x = c(1,0,1,1,0,1,0) 
y = c(1,1,0,1,0,0,1)
Hamming_distance(x,y)




#b)







###4)
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmean <- function(v) {
  uniqv <- sum(v)
  l <- length(v)
  return(uniqv/l)
}

#getmean(cr$X01)
cr <- read.csv("/Users/chanukya/Documents/GitHub/DataMining/HW1/crx.data",header=F)
summary(cr)



final <- function(cr){
cr$V1 <- as.factor(cr$V1)
cr$V2 <- as.numeric(cr$V2)
cr$V3 <- as.numeric(cr$V3)
cr$V4 <- as.factor(cr$V4)
cr$V5 <- as.factor(cr$V5)
cr$V6 <- as.character(cr$V6)
cr$V6 <- as.factor(cr$V6)
cr$V7<- as.character(cr$V7)
cr$V7 <- as.factor(cr$V7)
cr$V8 <- as.numeric(cr$V8)
cr$V9 <- as.factor(cr$V9)
cr$V10 <- as.factor(cr$V10)
cr$V11 <- as.numeric(cr$V11)
cr$V12 <- as.factor(cr$V12)
cr$V13 <- as.factor(cr$V13)
cr$V11 <-as.numeric(cr$V14)
cr$V15 <- as.numeric(cr$V15)
cr$V16 <- as.factor(cr$V16)


for (i in 1:length(colnames(cr))){
  for (j in 1:length(cr[,(colnames(cr)[i])])){
      if (class(cr[,(colnames(cr)[i])]) == "factor"){
         if (cr[j,i] == "?"){
          cr[j,i] <- getmode(cr[,(colnames(cr)[i])])
        }
      }
    if (class(cr[,(colnames(cr)[i])]) == "numeric"){
      if (cr[j,i] == "?"){
        cr[j,i] <- getmean(cr[,(colnames(cr)[i])])
      }
    }
  }
}
cr$V14[cr$V14 == "?"] <- getmean(cr$v14)
cr$V6[cr$V6 == "?"] <- getmode(cr$V6)
return(cr)
}   
cr <- final(cr)
summary(cr)

cr$V2 <- as.numeric(as.character(cr$V2))
cr$V3 <- as.numeric(as.character(cr$V3))
cr$V8 <- as.numeric(as.character(cr$V8))
cr$V11 <-as.numeric(as.character(cr$V11))
cr$V14 <-as.numeric(as.character(cr$V14))
cr$V15 <-as.numeric(as.character(cr$V15))
mydata <- as.numeric(cr$V2, cr$V3, cr$V8, cr$V11, cr$V14, cr$V15)
library(dplyr)

result <- cr %>%select(V2, V3, V8, V11, V14, V15)
set.seed(100)
result <- data.frame(result)

##2
#a
for ( i in 1:length(result)){
  print(class(result[,i]))
}
rando_sample <- result[sample(690, size=100, replace = T),]

View(rando_sample)
#b #c
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
pairs(rando_sample, histogram=TRUE, pch=19)
chart.Correlation(rando_sample, histogram=TRUE, pch=19)

#d

Normalize <- function(s1){
  for ( i in 1:length(s1)){
     s1[i] = ((s1[i]-mean(s1))/sqrt(sum((s1[i]-mean(s1))^2)))
  }
  return(s1)
}
rando_sample <- scale(rando_sample)


#e
#pairs(rando_sample)
chart.Correlation(rando_sample, histogram=TRUE, pch=19)



#3) It did effect the correlation. We can see that before normalization the data is less correlated and whereas after the normalization the some variables in data are highly correlated.




