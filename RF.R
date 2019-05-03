rm(list=ls())

library(randomForest)

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

randomF <- randomForest(form, type = "classification", ntree = 2000, data = data_train, importance = TRUE)
randomF
