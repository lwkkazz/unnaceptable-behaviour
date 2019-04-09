library (neuralnet)
library (data.table)
library (ROCR)
library (bit64)
library (Amelia)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

cdd <- "C:\\Users\\lucas\\Desktop\\Machine Learning\\Data\\"

data <- fread(paste0(cdd, "flying-etiquette.csv"), stringsAsFactors = T, na.strings="NA")
data[data == ''] <- NA
missmap(train)

colnames(data)
#13
train <- subset(data, select = c(2, 5, 13,
                                 24, 25, 26, 27))

colnames(train) <- c("often_travel", "height",  "recline",  "gender", "age", "income", "education")

train <- train[complete.cases(train), ]
train <- as.data.frame(lapply(train, unclass))
train <- as.data.frame(lapply(train, normalize))


test  <- tail(train, nrow(train) * 0.25)
train <- head(train, nrow(train) * 0.75)

start_time <- Sys.time()
nn <- neuralnet(recline~.,
                data=train, hidden=c(3, 3, 3))
end_time <- Sys.time()
time <- end_time - start_time

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