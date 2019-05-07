rm(list=ls())

library(randomForest)
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

randomF <- randomForest(form, type = "classification", ntree = 3000, data = data_train, importance = FALSE)

pred <- predict(randomF, newdata = data_test, type = "response")


results <- as.data.frame(data_test$F34)
colnames(results) <- c("actual")
results$predicted <- as.numeric(pred) - 1
results$hitmiss <- ifelse(((results$actual == 1 & results$predicted >= 0.5) | (results$actual == 0 & results$predicted < 0.5)),1,0)
mean(results$hitmiss)

ROCRpred <- prediction(results$predicted, results$actual)
ROCRperf <- performance(ROCRpred, 'tpr', 'fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2, 1.2))

auc_ROCR <- performance(ROCRpred, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR

