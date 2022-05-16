# Exemplo Bhaskara
a <- 2
b <- -16
c <- -18

v1 <- (-b + sqrt(b*b - 4*a*c)) / (2*a)
v2 <- (-b - sqrt(b*b - 4*a*c)) / (2*a)

#cat("v1:",v1,"v2:",v2,"\n")

#Exercicio Slide 4 - Página 16

dap <- 15
h <- 12
biomassa1 <- (exp(1)^-1.7953) * (dap^2.2974) 
biomassa2 <- exp(1) ^ (-2.6464 + (1.9960 * log(dap)) + (0.7558 * log(h)))

#cat("biomassa1:",biomassa1,"biomassa2:",biomassa2, "\n")

#Exercicio slide 5 - Página 38

consumo <- c(9839,10149,10486,10746,11264,11684,12082,12599,13004,13350,13717,14052)
names(consumo) <- month.abb
#cat("Média de consumo:",mean(consumo),"\n")
#cat("Consumo máximo:",max(consumo),"no mês:",names(consumo)[which.max(consumo)],"\n")
#cat("Consumo mínimo:",min(consumo),"no mês:",names(consumo)[which.min(consumo)],"\n")
#cat("Ordem decrescente:",sort(consumo, decreasing = TRUE),"\n")


#Exercicio slide 6 - Página 23

cidades <- matrix(0,4,4)
colnames(cidades) <- c("Atenas","Paris","Madri","Estocolmo")
rownames(cidades) <- c("Atenas","Paris","Madri","Estocolmo")
cidades["Atenas","Madri"] <- 3949
cidades["Atenas","Paris"] <-3000
cidades["Atenas","Estocolmo"] <-3927
cidades["Paris","Madri"] <-1273
cidades["Madri","Estocolmo"] <-3188
cidades["Paris","Estocolmo"] <-1827
cidades[,1] <- cidades[1,]
cidades[,2] <- cidades[2,]
cidades[,3] <- cidades[3,]
cidades[,4] <- cidades[4,]
print(cidades)

#Exercicio slide 8 - Página 21
v1<-c(2005:2016)
v2<-c(1:12)
v3<-c(1:31)

datas<-list(anos = v1, meses = v2, dias = v3)

#Exercicio slide 8 - Página 22
v<-c(1,3,4,7,11,18,29)
listaCalc <- list(`x*2`=v*2, `x/2`= v/2, `sqrt(x)` = sqrt(v))



