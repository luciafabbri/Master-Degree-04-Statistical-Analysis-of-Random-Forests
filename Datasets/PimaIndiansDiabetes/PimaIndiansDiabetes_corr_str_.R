library(tidymodels)
library(tidyr)
library(rpart)
library(randomForest)
library(foreach)
library(ggplot2)
library(geometry)
library(mlbench)

#open dataset
data("PimaIndiansDiabetes")
df <- PimaIndiansDiabetes
df <- na.omit(df)
head(df)

df$diabetes <- factor(ifelse(df$diabetes == 'pos', "1", "0"))
head(df)

# Split the data into training and testing sets
data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

nf <- 1:(ncol(train_data)-1) 
nt <- 50

get.stcor <- function(i){
  bio.rf <- randomForest(diabetes ~ ., type = 'classification', data = train_data, keep.inbag = T,
                         importance=TRUE, ntree = nt, mtry = f, replace = T, norm.votes = F)
  
  Qs <- as.data.frame(bio.rf$votes/bio.rf$oob.times) #proportion OOB votes cast at x for each class
  mr <- c()
  squared.mr <- c()
  sd_k <- c()
  
  #correction on the sign and quantities for the computation of the strength and correlation
  for(i in 1:nrow(train_data)){
    y <- train_data[i,'diabetes']
    
    mr[i] <- Qs[i, y] - max(Qs[i, !(names(Qs) %in% y)])
    squared.mr[i] <- (Qs[i, y] - max(Qs[i, !(names(Qs) %in% y)]))**2
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
        
        y <- train_data[r, 'diabetes'] # true class
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
  
  #correlation
  corr <- var.mr/(mean(sd_k)**2)
  return(c(strength, corr))
}

strength_by_nfeat <- c()
corr_by_nfeat <- c()

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


pe <- (corr_by_nfeat * (1 - strength_by_nfeat)**2) / (strength_by_nfeat**2)
pe

cs2 <- corr_by_nfeat/(strength_by_nfeat**2)


plot(nf, cs2,type="b", pch=19, xlab="number of features", ylab="UB of PE*")
axis(1, at=1:9)

