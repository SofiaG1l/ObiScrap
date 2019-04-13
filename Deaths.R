############################## libraris
library(foreign)
library(sqldf)
library(stringr)
library(reshape2)
library(dplyr)
library(Hmisc)
library(kableExtra)
library(stringi)

############################# get db
cont=1
for (i in 2010:2016) {
  #i=1995
  arch<-paste("U:/Data/DEFT",i,".dbf",sep="")
  if (i<2014) {
    db_i<-read.dbf(arch)[c("PAIRE", "PROVRE", 'DEPRE', "EDAD", "UNIEDA", "SEXO", "FECDEF", "FECNAC")]
  }
  if (i>=2014) {
    #caract fecha () 2014 y 2015 tienen la fecha con nros
    db_i<-read.dbf(arch)[c("PAISRE", "PROVRES", 'DEPRE', "EDAD", "UNIEDA", "SEXO", "FECDEF", "FECNAC")]
    db_i$PAIRE<-db_i$PAISRE
    db_i$PROVRE<-db_i$PROVRES
  }
  
  #gr_edad
  db_i$EDAD_r<-db_i$EDAD
  db_i$EDAD_r[db_i$UNIEDA==2 | db_i$UNIEDA==3 | db_i$UNIEDA==4 | db_i$UNIEDA==8]<-0
  
  if (i<2001) {
    #gr_edad
    db_i$EDAD_r<-db_i$EDAD
    db_i$EDAD_r[db_i$UNIEDA==2 | db_i$UNIEDA==3 | db_i$UNIEDA==4 | db_i$UNIEDA==8]<-0
    db_i$EDAD_r[db_i$UNIEDA==5] = 100
    db_i$EDAD_r[db_i$UNIEDA==9] = 999
  }
  #fecha de defunciones
  db_i$FECDEF=as.character(db_i$FECDEF)
  db_i$FECNAC=as.character(db_i$FECNAC)
  #set anio
  db_i$anio<-i
  #sexo
  db_i$SEXO[db_i$SEXO==9] <- 3
  #agrupo
  db_l<-sqldf("select PAIRE, PROVRE, DEPRE, SEXO, EDAD_r, anio, FECDEF, FECNAC, 
              count(anio) as ndef 
              from db_i 
              group by PAIRE, PROVRE, DEPRE, SEXO, EDAD_r, anio, FECDEF, FECNAC")
  if (cont==1){dbDex<-db_l}
  if (cont>1){dbDex<-sqldf("select * from db_l union all select * from dbDex")}
  cont=cont+1
  print(i)
}

############################ save db
dbD = dbDex

dbD$FECDEF = gsub(pattern = "[[:blank:]]", replacement = "", dbD$FECDEF)
dbD$FECDEF <- gsub(".", "/", dbD$FECDEF, fixed=TRUE)
dbD$dd <- colsplit(dbD$FECDEF, '/', names =  c('dd','mm', 'aa'))[,1]
dbD$mm <- colsplit(dbD$FECDEF, '/', names =  c('dd','mm', 'aa'))[,2]
dbD$aa <- colsplit(dbD$FECDEF, '/', names =  c('dd','mm', 'aa'))[,3]
dbD$fd_aa[is.na(dbD$aa)] <- dbD$anio[is.na(dbD$aa)]
summary(dbD$dd);summary(dbD$mm);summary(dbD$aa)
dbD$DDate = as.Date(paste0(dbD$aa,"/", dbD$mm,"/", dbD$dd), format = "%d/%m/%Y")

# chek missings
NI_aa = as.data.frame(xtabs(ndef~EDAD_r,dbD,is.na(aa) | aa==0 & anio>2013))
NI_mm = as.data.frame(xtabs(ndef~EDAD_r,dbD,is.na(mm)|mm==0& anio>2013))
NI_dd = as.data.frame(xtabs(ndef~EDAD_r,dbD,is.na(dd)|dd==0& anio>2013))
Total = as.data.frame(xtabs(ndef~EDAD_r,dbD))
library(tidyverse)
NI = list(Total, NI_dd, NI_mm, NI_aa) %>% reduce(left_join, by = "EDAD_r")
NI$x = as.integer(NI$x) 
colnames(NI) = c('x', 't', 'dd', 'mm', 'aa')
NI$dd_p = round(NI$dd/NI$t*100,2)
plot(NI$x[NI$x<100], NI$dd_p[NI$x<100], t='o')

######################## PERGAMINO
dbD$DEPRE = as.integer(dbD$DEPRE)
dbD$PROVRE = as.integer(dbD$PROVRE)

subset(dbD, aa==2015 & PROVRE==6 & DEPRE==623 & EDAD_r==51 & SEXO==1)
subset(dbD, aa==2015 & PROVRE==6 & DEPRE==623 & mm==1 & SEXO==1)


perga15 = subset(dbD, aa==2015 & PROVRE==6 & DEPRE==623 & EDAD_r==51 & SEXO==1)
table(perga15$dd)/table()
summary(perga15)


table(dbD$PROVRE,dbD$mm)[,1]/t(t(table(dbD$PROVRE)))
dim(table(dbD$PROVRE,dbD$dd))
dim(t(t(table(dbD$PROVRE))))














subset(dbD, mm==7 & aa==2014 & PROVRE==82 & EDAD_r==45 & SEXO==2)
