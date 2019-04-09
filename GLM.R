library (data.table)
library (ROCR)
library (bit64)
library (Amelia)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

cdd <- "C:\\Users\\lucas\\Desktop\\Machine Learning\\ProjetoML\\data\\"

data <- fread(paste0(cdd, "base_perfil.csv"), stringsAsFactors = T)

data[data == ''] <- NA
data[data == 'NA'] <- NA
data[data == 'N/A'] <- NA
data[data == 'Prefiro não responder'] <- NA

# missmap(data)

data <- as.data.frame(lapply(data, unclass))
data <- as.data.frame(lapply(data, normalize))
