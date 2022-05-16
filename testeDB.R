install.packages("RMySQL")
library(RMySQL)

mydb = dbConnect(MySQL(), user='root', password='bncs1520', dbname='testes_r', host='localhost')

dbListTables(mydb)
dbListFields(mydb, 'pessoa')

rs = dbSendQuery(mydb, "select * from pessoa")
dfpessoa = fetch(rs)