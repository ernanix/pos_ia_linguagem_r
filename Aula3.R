#Slide 13 - Página 25
#Gere os gráficos em arquivos PDF e PNG
setwd("D:\\Cursos\\Pos_IA\\linguagem_r")
png(file="teste.png")
ggplot(data.frame(x=c(0,25)),aes(x=x)) + stat_function(fun=func,geom="line")
dev.off()

pdf(file="teste.pdf")
ggplot(data.frame(x=c(0,25)),aes(x=x)) + stat_function(fun=func,geom="line")
dev.off()

#Efetuar a análise de regressão para os dados do arquivo GAGurine.csvque está no Moodle. 
#Mostre as estatísticas, equação de reta e plote o gráficos de dispersão com a reta, e os gráficos de resíduos
#•Este arquivo contém a medição de níveis de GAG (glicosaminoglicanos) na urina de crianças de certa idade
#•http://www.razer.net.br/datasets/GAGurine.csv

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