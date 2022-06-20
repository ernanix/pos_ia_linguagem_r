#https://uc-r.github.io/missing_values

#air <- airquality

#______________________________________________________________________________________________________
#Pesquisar Typora e Latex


# Slide 9 - Página 40
df <- data.frame(letras=letters[1:10],numeros=21:30,valores=rnorm(10))

# a)Retorne a linha 5
df[5,]
# b)Retorna a coluna 2 (como vetor e como data frame –drop=FALSE)
df[,2]
df[,2,drop=FALSE]
# c)Retorne as colunas 2 e 3
df[,c(2,3)]
# d)Retorne os elementos da linha 6, mas somente as colunas 1 e 3
df[6,c(1,3)]
# e)Retorne os elementos que possuem na coluna "valores" um valor maior que zero
df[df$valores > 0,]
# f)Retorne os elementos que possuem na coluna "numeros" um valor ímpar
df[df$numeros%%2 != 0,]
# g)Retorne os elementos que possuem na coluna "valores" um valor maior que zero e na coluna "numeros" um valor par
df[df$valores > 0 & df$numeros%%2 == 0,]
# h)Retorne os elementos que possuem na coluna "letras" somente os seguintes valores "b",  "g", "h"
df[df$letras %in% c("b","g","h"),]


# Slide 9 - Página 49
df1 <-data.frame(nome=c("Razer", "Anthom", "Nizer", "Rojas", "Montaño"), cidadeId=c(3, 10, 2, 3, 1))
cidades <-data.frame(cidadeId=c(1, 2, 3, 4), cidade=c("Curitiba", "SJP", "Pinhais", "Colombo"))

#a)Cross Join
merge(df1,cidades,by=NULL)
#b)InnerJoin
merge(df1,cidades,by="cidadeId")
#c)OuterJoin
merge(df1,cidades,by="cidadeId",all=TRUE)
#d)LeftOuterJoin
merge(df1,cidades,by="cidadeId",all.x=TRUE)
#e)RightOuterJoin
merge(df1,cidades,by="cidadeId",all.y=TRUE)

#slide 9 - Página 58

n<-10
sexo <-sample( c("masculino", "feminino"), n, replace=TRUE)
idade <-sample( 14:102, n, replace=TRUE)
peso <-sample( 50:90, n, replace=TRUE)
menor <-idade<18
pessoas <-data.frame(sexo=sexo, idade=idade, peso=peso, menor=menor, stringsAsFactors=FALSE)

#•Ordene o data frame por peso
pessoas[order(pessoas$peso),]
#•Ordene o data frame por sexo e peso, decrescentemente
pessoas[order(pessoas$sexo,-pessoas$peso),]
#•Dê a maior idade nos dados (max)
max(pessoas$idade)
#•Dê a média dos pesos (mean)
mean(pessoas$peso)
#•Mostrar as pessoas do sexo feminino que estão na base
pessoas[pessoas$sexo == 'feminino',]
#•Contar as pessoas do sexo feminino (nrow)
nrow(pessoas[pessoas$sexo == 'feminino',])


#Slide 10 - Página 22
#Carregue o arquivo http://www.razer.net.br/datasets/Biomassa_REG.csv
setwd("D:/Cursos/Pos_IA/linguagem_r")
biomassa<-read.csv2("Biomassa_REG.csv")

#Carregue o arquivo http://www.razer.net.br/datasets/fertility.csv
fert<-read.csv("fertility.csv")

#Salve a base de dados IRIS (data frameiris) usando os seguintes formatos:
  #a.Separador "**", ponto decimal ".", sem os nomes das linhas e o cabeçalho de colunas, 
  #com aspas nos campos string
write.table(iris,"iris1.txt",sep="**",dec=".",row.names=FALSE,col.names = FALSE)
  #b.CSV com ponto decimal ",", sem os nomes das linhas, com o cabeçalho de colunas e sem aspas nos campos string
write.table(iris,"iris1.csv",sep=";",dec=",",row.names=FALSE,col.names = TRUE,quote = FALSE)


#Slide 11 - Página 20
#Escreva um laço que varre os números de 1 a 7 e imprime seus quadrados, usando o comando print()
vetor <- 1:7
for (i in vetor) {
  print(i^2)
}

vetor2 <- rnorm(10)
#Usando laços, varra uma lista de números aleatórios gerados por rnorm(), mas pare se o número encontrado for mais que 1.
for (i in vetor2) {
  if (i > 1)
    break
  else
    print(i)
}

#Usando laços, varra uma lista de números aleatórios gerados por rnorm(), mas use o comando next para pular os números negativos
i<-1
while (i <= length(vetor2)) {
  if (vetor2[i] < 0) {
    i <- i+1
    next
  }
  print(vetor2[i])
  i <- i+1
}

#Use laços aninhados para criar a matriz abaixo. Faça a alocação prévia da matriz com valores NA
matriz <-matrix(data=NA,nrow=5,ncol=5)

for (l in 1:nrow(matriz)) {
  for (c in 1:ncol(matriz)) {
    if (l == c) {
      matriz[l,c] <- 0
    }
    if (l==c-1 || l-1==c){
      matriz[l,c] <- 1
    }
    if (l==c-2 || l-2==c){
      matriz[l,c] <- 2
    }
    if (l==c-3 || l-3==c){
      matriz[l,c] <- 3
    }
    if (l==c-4 || l-4==c){
      matriz[l,c] <- 4
    }
  }
}

#Crie o seguinte data frame
student.df <- data.frame( name = c("Sue", "Eva", "Henry", "Jan"),sex = c("f", "f", "m", "m"),years = c(21,31,29,19));

#Usando um comando ifelse(), crie uma coluna chamada teen, booleana, que indica se a pessoa possui menos de 20 anos.