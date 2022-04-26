#Initializing libraries
library(stats)
library(tidyverse)
library(ggplot2)
library(NbClust)
library(factoextra)

#Importing data 
mydata <- read.csv("C:/Users/user/Desktop/Mydata.csv",stringsAsFactors = FALSE)
view(mydata)
mydata.label = mydata$Country_code
mydata.label
summary(mydata)

mydata <- as_tibble(mydata)
mydata

library(tidyr)
#convert row names to column names to restructure data to suit analysis
mydata2 <- spread(mydata,key = Series, value = Year)
mydata2[is.na(mydata2)] <- 0
mydata2

#Selecting and scaling data for KMeans analysis
mydata2_d <- (mydata2[3:8])
mydata2_d


mydata2_scale <- scale(mydata2_d)
view(mydata2_scale)

#create clusters with kmeans
kmeans(mydata2_scale, centers = 4, iter.max = 100, nstart = 100, algorithm = "Lloyd",)

#Determine the optimum number of clusters using the within sum of squares method
fviz_nbclust(mydata2_scale, kmeans, k.max = 9, method = "wss")

#Create cluster biplot
fviz_cluster(kmeans(mydata2_scale, centers = 3, iter.max = 100, nstart = 100), data = mydata2_scale)

clusters <- kmeans(mydata2_scale, centers = 3, iter.max = 100, nstart = 100)
clusters
da <- clusters$cluster
da

mydata2 <- mydata2 |> mutate(clusters = da)
view(mydata2)


mydata2 |> ggplot(aes(x=ï..Country_Name, y=clusters)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("Country_code") +
  theme_bw()

#Libraries used in decison trees analysis
install.packages("rpart")
install.packages("repart.plot")

library(rpart)
library(rpart.plot)


#This set of function focus on creating a decision variable for predictive analysis
#Countries in cluster two(2) was set to one due to having higher average GDP growth
dec <- function(dax){
  for (i in 1:length(dax)) {
    if (dax[i]==2) {
      return(dax[i] <- 1)
      } else {
      return(dax[i] <- 0)
    }
  }
}

ad <- function(da){
  for (i in 1:length(da)) {
    da[i] = dec(da[i])
    return(da[i])
  }
}

ad(mydata2$clusters)

ah <- 0

for(i in 1:length(mydata2$clusters)){
  ah[i] = ad(mydata2$clusters[i])
  print(ah[i])
}

ah

#Creating categorical column
df<- mutate(mydata2, cat = ah)
df
view(df)

df2 <- select(df, -Country_code, -ï..Country_Name, -clusters)
view(df2)

#Logistic regression
install.packages("caTools")
library(caTools)
split <- sample.split(df,SplitRatio = 0.8)
split
training <- subset(df2 <- select(df, -Country_code, -ï..Country_Name, -clusters),split=="TRUE")
testing <- subset(df2 <- select(df, -Country_code, -ï..Country_Name, -clusters),split=="FALSE")

model  <- glm(cat~., training, family = "binomial")
summary(model)
