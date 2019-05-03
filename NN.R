rm(list=ls())

library(neuralnet)

library (data.table)
library (ROCR)
library (bit64)
library (Amelia)
library (na.tools)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

cdd <- "C:\\Users\\lucas\\Desktop\\Machine Learning\\ProjetoML\\data\\"

data <- fread(paste0(cdd, "base_perfil.csv"), stringsAsFactors = T, dec = ",")

data[data == ''] <- NA
data[data == 'NA'] <- NA
data[data == 'N/A'] <- NA
data$`Qual é a sua idade-` <- na.mean(data$`Qual é a sua idade-`, option = "mean")

summary(data)

data <- as.data.frame(lapply(data, unclass))

data$Qual.é.o.nível.de.escolaridade.de.seu.pai. <- NULL
data$Qual.é.o.nível.de.escolaridade.de.sua.mãe. <- NULL
data$Qual.é.o.tipo.de.vínculo.da.sua.atividade.remunerada..não.acadêmica.. <- NULL
data$Você.pretende.solicitar.transferência.para.outro.Bacharelado.Interdisciplinar.da.UFABC.no..próximo.ano.acadêmico.. <- NULL
data$No.atual.quadrimestre.letivo..você.cursa.disciplinas.em.qual.is..campus.i... <- NULL
data$No.atual.quadrimestre.letivo..em.qual.turno.você.frequenta.a.maioria.das.disciplinas.. <- NULL
data$Você.já.efetuou.trancamento.total.de.matrícula. <- NULL
data$Quantas.horas..em.média..você.permanece.na.UFABC.por.semana. <- NULL
data$Você.possui.a.intenção.de.iniciar.um.curso.de.pós.graduação.stricto.sensu..mestrado.ou.doutorado..logo.após.a.sua.graduação. <- NULL

data[is.na(data$Você.já.foi.reprovado.em.alguma.disciplina..), ] <- 0

# missmap(data)

data <- as.data.frame(lapply(data, normalize))

data$Trancamento <- as.factor(data$Trancamento)

data_train <- head(data, nrow(data)*0.7)
data_test  <- tail(data, nrow(data)*0.3)

summary(data_train)

nam <- names(data)
form <- as.formula(paste("Trancamento ~", paste(nam[!nam %in% "Trancamento"], collapse = " + ")))

#######################

nn <- neuralnet(form, data=data_train, hidden=c(2, 2), lifesign = "full")



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