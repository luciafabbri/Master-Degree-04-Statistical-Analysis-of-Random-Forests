library(tidymodels)
library(tidyr)
library(rpart)
library(randomForest)
library(foreach)
library(ggplot2)
library(geometry)
library(mlbench)

get.error <- function(i){
  bio.rf <- randomForest(Class ~ ., type = 'classification', data = train_data, 
                         importance=TRUE, ntree = nt, mtry = features, replace = T)
  
  
  y.hat <- predict(bio.rf, newdata = subset(test_data, select = -c(Class)))
  
  cm <- table(test_data$Class, y.hat)
  as.numeric((cm[2,1] + cm[1,2])/sum(cm))
}

# Mostra quali dataset sono disponibili nel pacchetto
#data(package = "mlbench")

# Open dataset
data("Ionosphere")
df <- Ionosphere
df <- na.omit(df)
head(df)

# Stampa dei valori unici di classificazione
  print(unique_values <- unique(df$Class)) # Class good - bad 

df$Class <- factor(ifelse(df$Class == 'good', "1", "0"))
head(df)

# Ricerca ed eliminazione delle variabili categoriche 
# (non ha senso fare one-hot encoding perchè non apportano un valore reale ??)
print(col_to_remove <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))])
col_to_remove <- c("V1", "V2")
df <- df[, !(names(df) %in% col_to_remove)]
head(df)

# Split the data into training and testing sets
data_split <- initial_split(df, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)


####################################################################
# 4-random forest using random input (attributes) selection
nt <- 100 # number of trees in each round
nf <- c(1:(ncol(df)-1)) # number of features considered in each split
errors_feat <- c()

for(features in nf){
  result <- t(sapply(1:100, get.error))
  errors_feat[features] <- mean(result)
}

errors_feat
ggplot() + geom_point(aes(x = nf, y = errors_feat))



######################################################################
# 4 - one tree for each random forest
nt <- 1
#different number of features
nf <- c(1:(ncol(df)-1)) # number of features extracted in each tree
errors_feat <- c()

for(features in nf){
  result <- t(sapply(1:100, get.error))
  errors_feat[features] <- mean(result)
}

errors_feat
ggplot() + geom_point(aes(x = nf, y = errors_feat))



######################################################################
# 5 - linear combination of inputs
L <- 3
r <- choose(ncol(df)-1,L)

#extract all the linear combinations from L features
lc <- data <- data.frame(matrix(NA,    # Create empty data frame
                                nrow = nrow(df),
                                ncol = 0))

combinations <- combn(colnames(subset(df, select = -c(Class))), L, simplify = F)

for(i in combinations){
  coef <- c(runif(L, -1,1))
  
  v <- unlist(i)
  
  lc[, paste("LC",paste(v, collapse = ''), sep = '')] <- df[v] %>%
    apply(., 1, function(x) dot(x, coef, d = T))

}

lc$Class <- df$Class

#random forest on the new dataset
# Split the data into training and testing sets
data_split <- initial_split(lc, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)


nt <- 100
#different number of features
nf <- c(1:(ncol(df)-1)) # number of features extracted in each tree
errors_feat <- c()

for(features in nf){
  result <- t(sapply(1:100, get.error))
  errors_feat[features] <- mean(result)
}

errors_feat
ggplot() + geom_point(aes(x = nf, y = errors_feat))

