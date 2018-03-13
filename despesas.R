setwd("eptc/")
list.files()
despesas <- read.csv("despesas-2018-03-1312_25_32.csv", header = T, sep = ";")
names(despesas) <- c("Entidade","Data","Pessoa","DOC","Nome","Processo","Despesa","NF", "Valor","ValorRetido","Liquido")
despesas$Entidade = NULL
despesas$Data = NULL
despesas$Pessoa = NULL
despesas$DOC = NULL
despesas$Nome = NULL
despesas$Processo = NULL
despesas$Valor <- gsub("\\.", "", despesas$Valor)
despesas$Valor <- gsub("\\,", ".", despesas$Valor)
despesas$Valor <- as.double(as.character(despesas$Valor))
library(readr)
options(scipen=999)
parse_double(despesas$Valor)
despesas$Valor <- as.double(despesas$Valor)
#install.packages("scales")
library(scales)
despesas_total <- dollar_format()(sum(despesas$Valor))
despesas_total




SALARIOS <- read.csv("SALARIOS.csv", header = T, sep = ";")
names(SALARIOS) <- c("Entidade","Data","Pessoa","DOC","Nome","Processo","Despesa","NF", "Valor","ValorRetido","Liquido")
SALARIOS$Entidade = NULL
SALARIOS$Data = NULL
SALARIOS$Pessoa = NULL
SALARIOS$DOC = NULL
SALARIOS$Nome = NULL
SALARIOS$Processo = NULL
SALARIOS$Valor <- gsub("\\.", "", SALARIOS$Valor)
SALARIOS$Valor <- gsub("\\,", ".", SALARIOS$Valor)
SALARIOS$Valor <- as.double(as.character(SALARIOS$Valor))
library(readr)
options(scipen=999)
parse_double(SALARIOS$Valor)
SALARIOS$Valor <- as.double(SALARIOS$Valor)
salarios_total <- dollar_format()(sum(SALARIOS$Valor))
salarios_total

somado <- rbind(despesas, SALARIOS)




#sinalizacao <- despesas[which(despesas$Despesa == "MATERIAL DE SINALIZACAO VIARIA"),names(despesas) %in% c("Data","Pessoa","Despesa","NF", "Valor","ValorRetido","Liquido")]
#sinalizacao$Valor <- as.double(sinalizacao$Valor)
#despesas_sinalizacao <-  dollar_format()(sum(sinalizacao$Valor))
#despesas_sinalizacao

custos <- despesas[which(despesas$Valor > 5000),names(despesas) %in% c("Data","Pessoa","Despesa","NF", "Valor","ValorRetido","Liquido")]
custos <- aggregate(Valor ~ Despesa, data=despesas, sum)
library(ggplot2)
ggplot(despesas, aes(x = Despesa, y = Valor)) + 
  geom_bar(stat = "identity", fill = "Pink",  las = 5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Tipos de Despesas") + ylab("Total de Despesas") + 
  ggtitle("Despesas EPTC 2017")


somado <- aggregate(Valor ~ Despesa, data=somado, sum)
library(ggplot2)
ggplot(somado, aes(x = Despesa, y = Valor)) + 
  geom_bar(stat = "identity", fill = "green",  las = 5) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  xlab("Tipos de Despesas") + ylab("Total de Despesas") + 
  ggtitle("Despesas EPTC 2017")

#View(custos)
custo_total <- dollar_format()(sum(somado$Valor))
custo_total
