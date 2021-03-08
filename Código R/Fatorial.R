# Carregando pacotes a serem utilizados

library(dplyr)
library(corrplot)
library(psych)
library(xtable)
library(sjmisc) #Rotação data frame


# Limpar----

rm(list=ls())


#Diretório

setwd("c:/Users/CAIO AZEVEDO/Documents/Documentos Caio/Github/Monografia")

# Importando os dados disponíveis no GitHub

#site<-"https://raw.githubusercontent.com/caio-azevedo/Monografia/master/data/base.csv"

dados<- read.table("data/base.csv", header=T, sep=";")


# Preparação dos dados

dados<-dados %>% 
  select(mun,x1, x2,x4, x5, x7, x8, x9,x12, x13, x15, x16, x17, x18)

df<-as.matrix(dados[,2:14])
row.names(df)<-dados$Municipio


df<-scale(df)

# Matriz de correlação

matcor<-cor(df)

corrplot(matcor, method="circle")

dev.copy(pdf,"Figuras/matriz_cor.pdf")
dev.off()


# Teste de Bartlett

cortest.bartlett(df)

# KMO

kmo<-KMO(df)

tab_kmo<-kmo$MSAi

tab1<-rotate_df(data.frame(tab_kmo), rn = c("Variável"))
tab1[1]<-"KMO"


# Variância Explicada

tab2<-summary(princomp(df,cor=TRUE))

tab2<-data.frame(tab2[["sdev"]], 
                row.names = c(1:13))
colnames(tab2)<-c("Desvio Padrão")

tab2<-tab2 %>% 
  mutate("Variância"=`Desvio Padrão`^2) %>% 
  mutate("Proporção"= Variância/sum(Variância))%>% 
   mutate("Acumulada"=cumsum(Proporção)) %>% 
  select(-"Desvio Padrão")



# Screeplot

var_tab2<-tab2$Variância

plot(var_tab2, lty=1, type="l",xlab = "Fator", ylab = "Variância", 
     pch=19, col="blue",xlim = c(1,8), lwd=2, bty="l")
abline(h = 1, col = "black", lty=2)
points(var_tab2,pch=18, cex=1.5)


dev.copy(pdf,"Figuras/screeplot.pdf")
dev.off()


# Rotação

PCAvarimax<-principal(df, nfactors=3,
                           n.obs=92,rotate="varimax",scores=TRUE)



# Comunalidades

tab<-rotate_df(data.frame(PCAvarimax[["communality"]]), rn = c("Variável"))
tab[1]<-"Comunalidade"



# Diagrama

fa.diagram(PCAvarimax, main = "")

dev.copy(pdf,"Figuras/componentes.pdf")
dev.off()


bartlett<-factor.scores(df,PCAvarimax, 
              method = c("Bartlett"))

prev<-data.frame(bartlett[["scores"]])


# Exportando os resultados

write.table(prev,file='data/base_fatorial.csv',sep=';',na="",
            quote=TRUE, row.names=TRUE, col.names = TRUE )

# Saídas Latex

print(xtable(tab1, caption = "Teste de Kaiser-Meyer-Olkin", 
             label = "tab1"),
      caption.placement = "top", 
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))

print(xtable(tab2, caption = "Sumário do Total de Variância Explicada", 
             label = "tab2", digits =3 ),
      caption.placement = "top",
      include.rownames = TRUE,
      format.args = list(big.mark = ".", decimal.mark = ","))


print(xtable(tab, caption = "Comunalidades", 
             label = "tab", digits =2 ),
      caption.placement = "top",
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))




