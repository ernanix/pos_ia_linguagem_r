#Slide 13 - Página 25
#Gere os gráficos em arquivos PDF e PNG
setwd("/Users/MPPR/Documents/Pos_IA/pos_ia_linguagem_r")
png(file="teste.png")
ggplot(data.frame(x=c(0,25)),aes(x=x)) + stat_function(fun=func,geom="line")
dev.off()

pdf(file="teste.pdf")
ggplot(data.frame(x=c(0,25)),aes(x=x)) + stat_function(fun=func,geom="line")
dev.off()



#Slide 13 - Página 63
#Efetuar a análise de regressão para os seguintes dados. Mostre as estatísticas,
#equação de reta e plote o gráficos de dispersão com a reta, e os gráficos de resíduos

x <- c(6.5,5.8,7.8,8.1,10.4,12.3,13.1,17.4,20.1,24.5,25.5,27.1)
y <- c(1.4,1.5,1.7,1.9,2.1,2.2,2.4,3.2,3.7,4.2,4.8,5.2)
modelo <- lm(y~x)
summary(modelo)
plot(x=x,y=y)
abline(modelo,col="red")
plot(resid(modelo))
abline(0,0,col="red")

#Efetuar a análise de regressão para os dados do arquivo GAGurine.csvque está no Moodle. 
#Mostre as estatísticas, equação de reta e plote o gráficos de dispersão com a reta, e os gráficos de resíduos
#Este arquivo contém a medição de níveis de GAG (glicosaminoglicanos) na urina de crianças de certa idade
#http://www.razer.net.br/datasets/GAGurine.csv

dados <- read.csv("GAGurine.csv")
dados$X <-NULL
modelo <- lm(dados$GAG ~ dados$Age)
coef(modelo)
resid(modelo)
plot(x=dados$Age,y=dados$GAG,xlab = "Age",ylab = "GAG")
abline(modelo,col="red")

plot(resid(modelo))
abline(0,0,col="red")

#Slide 13 - Página 36

#Para os dados CoolingWater, gere as estimativas com um polinômio de grau 3 e de grau 4. 
#Compare as curvas de predição.
dados <- read.csv("CoolingWater.csv")

#Grau 2
modelo <-lm(temp ~ poly(time, degree=2), data=dados)
plot(dados$temp ~ dados$time)
predicao <- predict(modelo,dados)
lines(y=predicao,x=dados$time, type="l", col="red")

#Grau 3
modelo <-lm(temp ~ poly(time, degree=3), data=dados)
plot(dados$temp ~ dados$time)
predicao <- predict(modelo,dados)
lines(y=predicao,x=dados$time, type="l", col="red")

#Grau 4
modelo <-lm(temp ~ poly(time, degree=4), data=dados)
plot(dados$temp ~ dados$time)
predicao <- predict(modelo,dados)
lines(y=predicao,x=dados$time, type="l", col="red")

#Use a base cars do R
#• Gere modelos polinomiais de graus 2, 3, 4 e 5
#• Faça as predições com cada modelo
#• Plote os pontos dos dados
#• Plote as linhas de cada uma das predições efetuadas, em cores diferentes

modeloG2 <- lm(dist ~ poly(speed, degree=2), data=cars)
modeloG3 <- lm(dist ~ poly(speed, degree=3), data=cars)
modeloG4 <- lm(dist ~ poly(speed, degree=4), data=cars)
modeloG5 <- lm(dist ~ poly(speed, degree=5), data=cars)

predicaoG2 <- predict(modeloG2,cars)
predicaoG3 <- predict(modeloG3,cars)
predicaoG4 <- predict(modeloG4,cars)
predicaoG5 <- predict(modeloG5,cars)

plot(cars$dist ~ cars$speed)
lines(y=predicaoG2,x=cars$speed,type="l",col="red")
lines(y=predicaoG3,x=cars$speed,type="l",col="yellow")
lines(y=predicaoG4,x=cars$speed,type="l",col="blue")
lines(y=predicaoG5,x=cars$speed,type="l",col="green")

#Slide 14 - Página 12

#Efetuar o exercício de classificação apresentado, usando a base IRIS.
#a) Apresente os resultados dos modelos
#b) Apresente o modelo que deu o melhor resultado

#Carrega o dataset
dataset <- iris

#Particiona o dataset
indices <- createDataPartition(dataset$Species, p=0.80, list=FALSE)
treino <- dataset[indices,]
teste <- dataset[-indices,]

#RandomForest
#Treino
rf <- train(Species~., data=treino, method="rf")
#Predição
predicoes.rf <- predict(rf, teste)
#Matriz de Confusão
confusionMatrix(predicoes.rf, teste$Species)

#SVM
#Treino
svm <- train(Species~., data=treino, method="svmRadial")
#Predição
predicoes.svm <- predict(svm, teste)
#Matriz de Confusão
confusionMatrix(predicoes.svm, teste$Species)


#####
library(mlbench)
library(caret)
library(mice)

data(BreastCancer)
temp_dados <- BreastCancer
temp_dados$Id <- NULL
#mice -> Analisa os dados e gera dados plausíveis a serem colocados nos Na's
imp <- mice(temp_dados)
# complete -> Completa os dados com o 1° conjunto plausível de dados
dados <- complete(imp, 1)


indices <- createDataPartition(dados$Class, p=0.80,
                               list=FALSE)
treino <- dados[indices,]
teste <- dados[-indices,]

set.seed(7)

#Treinar RF, SVM e RNA com a base de Treino
rf <- train(Class~., data=treino, method="rf")
svm <- train(Class~., data=treino, method="svmRadial")
rna <- train(Class~., data=treino, method="nnet",trace=FALSE)

#Aplicar modelos treinados na base de Teste
predict.rf <- predict(rf, teste)
predict.svm <- predict(svm, teste)
predict.rna <- predict(rna, teste)

#Verificar a quantidade de amostrar de cada classe na base de Teste
table(teste$Class)

#Criar as matrizes de confusão e comparar os resultados
confusionMatrix(predict.rf, teste$Class)
confusionMatrix(predict.svm, teste$Class)
confusionMatrix(predict.rna, teste$Class)

print(rna)

library(nnet)
final_model<-nnet(Class~.,data=dados,size=1,decay=0.1)
final_predict.rna <- predict(final_model, dados)
confusionMatrix(final_predict.rna, dados$Class)

library(kernlab)
final_model <- ksvm(type="C-svc", Class~., data=dados, kernel="rbfdot",
                    C=1.0, kpar=list(sigma=0.01173596))
final_predict.svm <- predict(final_model, dados)
confusionMatrix(final_predict.svm, dados$Class)

saveRDS(final_model, "cancer_mama_svm.rds")


###################Regressão

df_rs <-read.csv("C:/Users/escneto/Documents/Estudos/Pos_IA_UFPR/Linguagem_Programacao_R/real_state_tw.csv",sep=";",dec=",")

set.seed(7)

indices <- createDataPartition(df_rs$Y.house.price.of.unit.area, p=0.80, list=FALSE)
treino <- df_rs[indices,]
teste <- df_rs[-indices,]

# treino e predição - RandomForest
rf <- train(Y.house.price.of.unit.area~.,data=treino,method="rf")
predicoes.rf <- predict(rf,teste)

# treino e predição - SVM
svm <- train(Y.house.price.of.unit.area~.,data=treino,method="svmRadial")
predicoes.svm <- predict(svm,teste)

# treino e predição - RNA (Redes Neurais)
rna <- train(Y.house.price.of.unit.area~.,data=treino,method="nnet",trace=FALSE)
predicoes.rna <- predict(rna,teste)

# treino e predição - MLP (Perceptron MultiLayers)
install.packages("RSNNS")
library(RSNNS)
mlp <- mlp(treino[,1:7], treino[,8], linOut=T)
predicoes.mlp <- predict(mlp,teste[,1:7])

#Obtém metricas de cada modelo
rmse.rf <- RMSE(predicoes.rf, teste$Y.house.price.of.unit.area)
rmse.svm <- RMSE(predicoes.svm, teste$Y.house.price.of.unit.area)
rmse.rna <- RMSE(predicoes.rna, teste$Y.house.price.of.unit.area)
rmse.mlp <- RMSE(predicoes.mlp, teste$Y.house.price.of.unit.area)

rmse.rf
rmse.svm
rmse.rna
rmse.mlp

#RMSE - Root Mean Squared Error - Quanto menor, menos erro cometido
#Melhor - rmse.rf
