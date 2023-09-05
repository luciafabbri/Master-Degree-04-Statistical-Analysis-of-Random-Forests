library(MLDataR)
library(tidymodels)
library(tidyr)
library(rpart)
library(randomForest)
library(foreach)
library(ggplot2)
library(geometry)
library(zeallot)
library(dplyr)
library(plyr)

#open dataset
data("stroke_classification")
df <- stroke_classification[-c(1)]
df <- na.omit(df)

# Convert 1s to a positive label and 0s to a negative label
df$stroke <- factor(ifelse(df$stroke == 1, "0", "1"))


# Split the data into training and testing sets
data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

nf <- 1:(ncol(train_data)-1)
nt <- 100

set.seed(1234)

get.stcor <- function(i){
  bio.rf <- randomForest(stroke ~ ., type = 'classification', data = train_data, keep.forest = TRUE,
                         importance=TRUE, ntree = nt, mtry = f, replace = T, norm.votes = F, keep.inbag = T)
  
  
  
  Qs <- as.data.frame(bio.rf$votes/bio.rf$oob.times) #proportion OOB votes cast at x for each class
  mr <- c() # margin for the random forest
  squared.mr <- c()
  sd_k <- c()
  
  #correction on the sign and quantities for the computation of the strength and correlation
  for(i in 1:nrow(train_data)){
    y <- train_data[i,'stroke']
    
    mr[i] <- Qs[i, y] - max(Qs[i, !(names(Qs) %in% y)])
    squared.mr[i] <- (Qs[i, y] - max(Qs[i, !(names(Qs) %in% y)]))**2
    s1 <- s1 + bio.rf$votes[i, y]
    s2 <- s2 + max(bio.rf$votes[i, !(names(Qs) %in% y)])
    
  }
  
  #strength
  strength <- mean(mr)
  #E[quadro]
  var.mr <- mean(squared.mr) - strength**2
  
  train_predict <- predict(bio.rf, newdata = train_data, predict.all = T)$individual
  
  ####################
  
  
  for(k in 1:nt){
    s1_k <- 0
    s2_k <- 0
    n_oob <- sum(bio.rf$inbag[, k] == 0)
    for(r in row.names(train_predict)){
      #if the observation is not in the training set extracted at time k
      if(bio.rf$inbag[r, k] == 0){
        
        y <- train_data[r, 'stroke'] # true class
        ddf <- plyr::count(train_predict[r,1:k][train_predict[r,1:k] != y])
        j <- ddf[order(ddf$freq), 1] 
        
        
        if(train_predict[r,k] == y){ s1_k <- s1_k + 1}
        else if(train_predict[r,k] == j){ s2_k <- s2_k + 1}
      }
    }
    p1 <- s1_k / n_oob
    p2 <- s2_k / n_oob
    sd_k[k] <- sqrt(p1 + p2  + (p1 - p2)**2)
  }
  
  ####################
  
  #correlation
  corr <- var.mr/(mean(sd_k)**2)
  return(c(strength, corr))
}


for(f in nf){
  result <- t(sapply(1:100, get.stcor))
  strength_by_nfeat[f] <- mean(result[,1])
  corr_by_nfeat[f] <- mean(result[,2])
}

makePlot<-function(){
  plot(nf, strength_by_nfeat, ylim = c(0,1), type="b", pch=19, col="red", xlab="x", ylab="y")
  lines(nf, corr_by_nfeat, pch=18, col="blue", type="b", lty=2)
}

makePlot()
axis(1, at=1:9)
legend('center', legend=c("Strength", "Correlation"),
       col=c("red", "blue"), lty=1:2, cex=0.7,
       text.font=4, bg='lightblue')
