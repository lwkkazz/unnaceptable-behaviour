rm(list=ls())

library(neuralnet)

library (data.table)
library (ROCR)
library (bit64)
library (na.tools)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# cdd <- "C:\\Users\\lucas\\Desktop\\Machine Learning\\ProjetoML\\data\\"
cdd <- paste0(path.expand("~"), "/Documentos/proj/data/")

data <- fread(paste0(cdd, "base_perfil.csv"), stringsAsFactors = T, dec = ",")

origNames <- colnames(data)

data[data == ''] <- NA
data[data == 'NA'] <- NA
data[data == 'N/A'] <- NA

data <- as.data.frame(lapply(data, unclass))

names <- c()
nas   <- c()
for(i in 1:ncol(data)){
  names[i] <- paste0("F", i)
  nas[i]   <- sum(is.na(data[i]))
}
colnames(data) <- names
# 
# for(i in 1:ncol(data)){
#   message(paste0(i, ": ", nas[i], " = ", origNames[i]))
# }

data$F2   <- NULL
data$F3   <- NULL
data$F31  <- NULL
data$F33  <- NULL
data$F47  <- NULL
data$F5  <- na.mean(data$F5, option = "mean")

data[is.na(data$F28), ] <- 0
data[is.na(data$F29), ] <- 0
data[is.na(data$F32), ] <- 0
data[is.na(data$F38), ] <- 0
data[is.na(data$F39), ] <- 0

# missmap(data)

data <- as.data.frame(lapply(data, normalize))

data$F34 <- as.factor(data$F34)

data_train <- head(data, nrow(data)*0.7)
data_test  <- tail(data, nrow(data)*0.3)

summary(data_train)

nam <- names(data)
form <- as.formula(paste("F34 ~", paste(nam[!nam %in% "F34"], collapse = " + ")))


#######################

nn <- neuralnet(form, data=data_train, hidden=c(2, 2))

temp_test <- test

nn.results <- compute(nn, temp_test)
results <- data.frame(actual = test$recline, prediction = nn.results$net.result)


roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)


pred <- predict(nn, test, type="response")
ROCRpred <- prediction(pred, test$recline)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.2))

test$predicted <- pred

test$hitmiss <- ifelse(((test$recline == 1 & test$predicted >= 0.5) | (test$recline == 0 & test$predicted < 0.5)),1,0)
mean(test$hitmiss)
plot(nn)

auc_ROCR <- performance(ROCRpred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]

message(paste0("Neural network train time: ", time))