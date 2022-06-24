#Slide 13 - Página 25
#Gere os gráficos em arquivos PDF e PNG
setwd("D:\\Cursos\\Pos_IA\\linguagem_r")
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
