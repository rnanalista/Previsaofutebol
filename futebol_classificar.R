#################################################################################
# Este mini projeto tem como objetivo criar um modelo supervisionado que consiga#
# prever se um time vai ganhar, empatar ou perde o próximo jogo com base nas    #
# estáticas das últimas partidas dele.                                          #
#################################################################################
library(dplyr)
library("lubridate")
library(matrixStats)
library(psych)
# Defenir diretório
setwd("E:/futebol/projeto")

# importando CSVs do campeonato brasileiro de 2019 a até a rodada 6 de 2022
df2019 <- read.csv("2019.csv", encoding = "UTF-8", sep = ";")
df2020 <- read.csv("2020.csv", encoding = "UTF-8", sep = ";")
df2021 <- read.csv("2021.csv", encoding = "UTF-8", sep = ";")
df2022 <- read.csv("2022.csv", encoding = "UTF-8", sep = ";")

# Combinando Csv
df <- rbind(df2019,df2020)
df <- rbind(df, df2021)
df <- rbind(df, df2022)
#View(df)
write.csv(df,"df_combinados.csv",fileEncoding = "UTF-8", row.names = FALSE)

###############################################################################
df <- read.csv("df_combinados.csv", encoding = "UTF-8")
# adicionando duas colunas (resultado-casa e resultado-fora)
df <- df %>% mutate(resultado.casa = ifelse(df$gol.casa > df$gol.fora, "venceu",
                                            ifelse(df$gol.casa == df$gol.fora, "empatou", "perdeu")
), .after = 13)
df <- df %>% mutate(resultado.fora = ifelse(df$gol.fora > df$gol.casa, "venceu",
                                            ifelse(df$gol.fora == df$gol.casa, "empatou", "perdeu")
))


# Separando o dataframe pelas colunas casa e fora e depois combinando as duas partes pelas linhas
# para que o data frame tenha os dados por jogo de cada time
nomeC <- c(
  'data',
  'time',
  'gol',
  'chutes',
  'chutesgol'
  ,'possebola',
  'passes',
  'precisaopasses',
  'faltas',
  'cartaoamarelo'
  ,'cartaovermelho',
  'impedimentos', 
  'escanteios'
  ,'resultado'
)
# Criando a primeira parte 
dfdata <- df[,1] # somente a coluna data
df1 <- df[,2:14] # somente as colunas casa
df1 <- cbind(dfdata, df1) # combinando o df1 com dfdata
colnames(df1) <- nomeC # renomeando as colunas
df1$estadio <- "casa" # adicionando a coluna estadio para definir se o jogos esta jogando em casa ou fora
#View(df1)

# Criando a segunda parte
df2 <- df[,15:27]
df2 <- cbind(dfdata, df2)
colnames(df2) <- nomeC
df2$estadio <- "fora"
#View(df2)

df <- rbind(df1,df2) # Combinando as duas partes pelas linhas
df <- df[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,15,14)] # reordenando


# Formantando e mudando o tipo das colunas 'possebola' e 'precisaopasses'
df$possebola <- as.integer(gsub("%", "", df$possebola))/100
df$precisaopasses <- as.integer(gsub("%", "", df$precisaopasses))/100

# Criando nova coluna para informar o dia da semana (segunda, terça, quarta.. etc.)
dfDiaSemana <- format(as.POSIXct(df$data), format="%Y-%m-%d")
diaSemana <- wday(dfDiaSemana, label = TRUE)
diaSemana <- data.frame(diaSemana)
df <- mutate(df, diaSemana, .after = 1)


# Criando a coluna que define quantos chutes precisa para chutar no gol
df <- df %>%
  mutate(nchutenogol = round(df$chutesgol/df$chutes,2), .after = 4)

# Criando a coluna que define quantos chutes precisa para fazer gol
df <- df %>%
  mutate(nchutefezgol = round(df$gol/df$chutes,2), .after = 1)

#View(df)
write.csv(df,"df_por_jogos.csv",fileEncoding = "UTF-8", row.names = FALSE)


# Para prever o próximo jogo precisamos pegar dados de jogos anteriores.
# Então será criado um novo Data Frame com a mediana das colunas númericas dos ultimos 3 jogos e
# e será mantido os dados das colunas factor do jogo atual do time.
# Desta forma será possível pegar os dados dos últimos três jogos e mais os dados da proxima partida de um time para prever 
# se o time vence, empata ou perde.
###############################################################################
df <- read.csv("df_por_jogos.csv", encoding = "UTF-8", stringsAsFactors = TRUE)
times <- unique(df$time) # obtendo os times
dfM = data.frame() # novo Data Frame
for (time in times) { # loop por time
  dfTime <- df[ifelse(df$time == time, TRUE,FALSE),] #criando Data frame temporário para o time atual
  dfTime[order(dfTime$data, decreasing=FALSE), ] # ordenando
  colunas_numericas <- sapply(dfTime, is.numeric) # pegando apenas colunas numéricas
  colunas_factor <- sapply(dfTime, is.factor) # pegando apenas colunas factores
  for (linha in 1:nrow(dfTime)) { # loop para percorrer cada linha do Data Frame temporário
    # Calcula a mediana dos ultimos 3 jogos do time  
    if(linha >= 4){
      y <- t(
        data.frame(
          colMedians(as.matrix(dfTime[(linha-3):(linha-1),colunas_numericas]), useNames = TRUE))
        )
      rownames(y) <- NULL
      y <- cbind(y, dfTime[linha,colunas_factor ])
      dfM <- rbind(dfM, y) # adiciona a linha nova com as medianas no Data Frame novo
    }
  }
}
dfM <- data.frame(lapply(dfM, function(x) if(is.numeric(x)) round(x, 2) else x)) # arredondando para duas casa decimais

###################################################################################
write.csv(dfM,"df_por_jogos_medianas.csv",fileEncoding = "UTF-8", row.names = FALSE)

#Criando a versão normalizada do novo Data Frame
normalizar <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
colunas_factor <- sapply(dfM, is.factor)
colunas_numericas <- sapply(dfM, is.numeric)
dfBCKP <- dfM
dfM <- lapply(dfM[,colunas_numericas], normalizar)
dfM$data <- dfBCKP$data
dfM$diaSemana <- dfBCKP$diaSemana
dfM$time <- dfBCKP$time
dfM$estadio <- dfBCKP$estadio
dfM$resultado <- dfBCKP$resultado
dfM <- as.data.frame(dfM, stringsAsFactors = FALSE)
########################################################################################
write.csv(dfM,"df_por_jogos_medianaas_normalizado.csv",fileEncoding = "UTF-8", row.names = FALSE)

########### Analisando o Data Frame ###################################################
df <- read.csv("df_por_jogos_medianas.csv", encoding = "UTF-8", stringsAsFactors = TRUE)
dfnomalizado <- read.csv("df_por_jogos_medianaas_normalizado.csv", encoding = "UTF-8", stringsAsFactors = TRUE)
#View(df)

# Verificando valores Na
colSums(is.na(df))
colSums(is.na(dfnomalizado))

# Tipo de dados
str(df)
str(dfnomalizado)

# Analisando a Correlação entre as variáveis numéricas
corPlot(df[,colunas_numericas], cex = 1.2)

# Mostrando a distribuição de cada Coluna
for (colnomes in colnames(df[,colunas_numericas])) {
  print("#################")
  print(colnomes)
  print(summary(as.factor(df[,colnomes])))
  print("-----------------")
  print(summary(df[,colnomes]))
}

for (colnomes in colnames(df[,colunas_factor])) {
  barplot(summary(df[,colnomes]))
  print("#################")
  print(colnomes)
  print(summary(df[,colnomes]))
}

############ Dividindo Data Frame e treinando ####################
library(caret)
library(randomForest)
library(rpart)
library(e1071)
library(ROSE)
# dividindo Data Frame em treino e teste
# poderá escolher entre o normalizado ou não
df <- df[, -14]
#df <- dfnomalizado[,-14]

linhas <- sample(1:nrow(df), 0.70 * nrow(df))
dados_treino <- df[linhas,]
dados_teste <- df[-linhas,]
summary(dados_treino$resultado)
summary(dados_teste$resultado)
#View(dados_treino)


# Random forest
modelo <- randomForest( resultado ~ . 
                        #-gol
                        #-possebola
                        #-faltas
                        #-impedimentos
                        #-diaSemana
                        #chutes
                        #passes
                        #-cartaoamarelo
                        #escanteios
                        #-time
                        #chutesgol
                        #-precisaopasses
                        #-cartaovermelho
                        #-estadio
                        , 
                        data = dados_treino, 
                        ntree = 200, nodesize = 400, importance = T)
varImpPlot(modelo)
pred <- predict(modelo, dados_teste[,-17], type='class')
caret::confusionMatrix(dados_teste$resultado, pred)

# rpart
modelo_rf_v3 = rpart(resultado ~ .
                     #-gol
                     #-possebola
                     #-faltas
                     #-impedimentos
                     #-diaSemana
                     #chutes
                     #passes
                     #-cartaoamarelo
                     #escanteios
                     #-time
                     #chutesgol
                     #-precisaopasses
                     #-cartaovermelho
                     #-estadio
                     ,
                     data = dados_treino,
                     control = rpart.control(cp = .0010)) 
pred <- predict(modelo_rf_v3, dados_teste[,-17], type='class')
caret::confusionMatrix(dados_teste$resultado, pred)


#naiveBayes
modeloV2 <- naiveBayes(resultado ~ .
                       #-gol
                       #-possebola
                       #-faltas
                       #-impedimentos
                       #diaSemana
                       #chutes
                       #passes
                       #-cartaoamarelo
                       #escanteios
                       #-time
                       #-chutesgol
                       #-precisaopasses
                       #-cartaovermelho
                       #-estadio
                       , 
                       data = dados_treino, laplace = 6)

pred <- predict(modeloV2, dados_teste[,-17], type='class')
caret::confusionMatrix(dados_teste$resultado, pred)


# modelo e1071
modelo_svm_v1 <- svm(resultado ~ . 
                     #-gol
                     #-possebola
                     #-faltas
                     #-impedimentos
                     #diaSemana
                     #chutes
                     #passes
                     #-cartaoamarelo
                     #escanteios
                     #-time
                     #-chutesgol
                     #-precisaopasses
                     #-cartaovermelho
                     #-estadio
                     , 
                     data = dados_treino, 
                     type = 'C-classification', 
                     kernel = 'polynomial') 
pred <- predict(modelo_svm_v1, dados_teste[,-17])
caret::confusionMatrix(dados_teste$resultado, pred)


#################################################################################
# A taxa de acertos dos modelos ficaram entre 0.3918 e 0.489. São resultados    #
# considerados ruins, já que todos ficaram muito abaixo dos 0.70 que seria um   #
# o resultado ideal.                                                            #
#                                                                               #
# A aleatoriedade no futebol é muito grande e isso fica claro nos resultados    #
# dos modelos. Não é possível prever o resultado da próxima partida de um time  #
# com base nas estatísticas das últimas partidas dele.                          #
#                                                                               #
# Obs: foram feitas inúmeras otimizações, mas não houve melhorias nos modelos.  #
#################################################################################
