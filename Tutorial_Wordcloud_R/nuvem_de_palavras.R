#===============================================================================
#INSTALACAO E ATIVACAO DE PACOTES
#===============================================================================
#install.packages("textreadr")
#install.packages("tm")
#install.packages("stopwords")
#install.packages("worcloud")
library(textreadr)
library(tm)
library(stopwords)
library(wordcloud)
palavras <- read.table("C:/Users/Carla/Desktop/Projeto Prospecta/stopwords.txt",header=F)
#===============================================================================


#===============================================================================
#LEITURA DOS ARQUIVOS
#===============================================================================
#Retirados de: http://www.redebim.dphdm.mar.mil.br/pergamum/biblioteca/index.php

#Leitura do resumo da Dissertacao 1
doc1 <- read_pdf("C:/Users/carla/OneDrive/Área de Trabalho/Nuvem de palavras/dissertacaoegn.pdf")
doc1
doc1$text == "RESUMO" #verificando a partir de que linha esta o resumo
resumo1 <- doc1$text[115:127] #armazenando o resumo da dissertacao 1
resumo1 <- paste(resumo1,collapse = " ") #armazenando o resumo da dissertacao 2
resumo1
#===============================================================================


#===============================================================================
#PROCESSO DE LIMPEZA E TRATAMENTO DOS DADOS
#===============================================================================
corpus1 <- gsub(pattern = "\\W",replace = " ", resumo1)
corpus1 <- iconv(corpus1,"UTF-8",to="ASCII//TRANSLIT")
corpus1 <- gsub(pattern="\\b[A-z]\\b{1}",replace= " ",corpus1)
corpus1 <- VectorSource(corpus1)
corpus1 <- VCorpus(corpus1) 
corpus1 <- tm_map(corpus1,tolower)
corpus1 <- tm_map(corpus1,removePunctuation)
corpus1 <- tm_map(corpus1,removeNumbers)
corpus1 <- tm_map(corpus1,removeWords,stopwords("pt"))
corpus1 <- tm_map(corpus1,removeWords,palavras[,1])
#corpus1 <- tm_map(corpus1,removeWords,remover)
corpus1 <- tm_map(corpus1,stripWhitespace)
inspect(corpus1)


#Juncao de palavras e sinonimo
for (j in seq(corpus1)){
  corpus1[[j]] = gsub("sul americanos", "sul_americanos", corpus1[[j]])
  corpus1[[j]] = gsub("politica externa", "politica_externa", corpus1[[j]])
  corpus1[[j]] = gsub("referencial teorico", "referencial_teorico", corpus1[[j]])
  corpus1[[j]] = gsub("documentos oficiais", "documentos_oficiais", corpus1[[j]])
  corpus1[[j]] = gsub("conflitos armados", "conflitos_armados", corpus1[[j]])
}  
t1 <- corpus1

m1   = tm_map(t1,PlainTextDocument)
mtd1 = DocumentTermMatrix(m1) #termos nas linhas e documento na coluna
dim(mtd1) #matriz com 274 termos e 1 documento
Terms(mtd1) #exibe os termos

#Verificando a frequencia com que os termos aparecem
freqt1<- colSums(as.matrix(mtd1))   
freqt1
length(freqt1) #228 termos   

#Verificando os termos e suas frequencias
R1 <- order(freqt1,decreasing = T) #R recebe os indices dos termos de menor ate maior freq
freqt1[R1]
table(freqt1)  #tabela que mostra a quantidade de vezes que os termos aparecem
table(freqt1[R1] == 1) #175 termos aparecem apenas uma vez

mfreq1 <- data.frame(names(freqt1[R1]),as.integer(freqt1[R1]))
names(mfreq1) <- c("termos","freqabs")
mfreq1
write.csv2(mfreq1,"C:/Users/carla/OneDrive/Área de Trabalho/Nuvem de palavras/freqtexto1.csv")

#NUVEM DE PALAVRAS DO TEXTO 1
freqt1 <- read.csv2("C:/Users/carla/OneDrive/Área de Trabalho/Nuvem de palavras/freqtexto1.csv",sep=";")
freqg1 <- as.data.frame(freqt1)
nomes1 <- as.character(freqg1[,2])
valor1 <- as.integer(freqg1[,3])
wordcloud(nomes1, valor1, min.freq = 1, scale = c(2,.0), random.order = F,colors=brewer.pal(8,"Dark2"))


