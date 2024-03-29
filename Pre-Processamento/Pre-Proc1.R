#Criando uma arvore de decis�o com o pacote rpart
#Primeiro: Modelo sem altera��es

#Instala��o e carrega os pacotes
library(rpart)
library(caret)

#Carrega os dados historicos
credito <- read.csv(file.choose(), sep = ",", header = TRUE) 

#Cria as parti��es - dados de treino e teste
particao <- createDataPartition(1:1000, p=.7)
creditotreino <-credito[particao$Resample1, ]
creditoteste <- credito[- particao$Resample1, ]

#Cria o Modelo ->formula (class ~ . , data="creditotreino, method="class)
modelo1 <- rpart(class ~ . , data = creditotreino, method = "class")

#Visualizando o modelo graficamente
plot(modelo1)
text(modelo1)

#Gerando uma previs�o para o modelo
previsao <- predict(modelo1, newdata = creditoteste)
head(previsao)



#Transformando em dataframe a previsao
previsao <- as.data.frame(previsao)

#Fazendo uma compara��o
previsao$class <- ifelse(previsao$bad >= .5, "bad", "good")
head(previsao)
fix(previsao)

#Transformando em fator
previsao$class = as.factor(previsao$class)

#Gerando Matriz de confus�o
confusao <- confusionMatrix(previsao$class, creditoteste$class)
print(confusao)