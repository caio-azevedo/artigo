# Carregando pacotes a serem utilizados

library(dplyr)
library(corrplot)
library(psych)
library(xtable)
library(sjmisc) #Rota��o data frame


# Limpar----

rm(list=ls())


#Diret�rio

setwd("c:/Users/CAIO AZEVEDO/Documents/Documentos Caio/Github/artigo")

# Importando os dados dispon�veis no GitHub

site<-"https://raw.githubusercontent.com/caio-azevedo/artigo/main/data/base.csv"

dados<- read.table(site, header=T, sep=";")


# Prepara��o dos dados

dados<-dados %>% 
  select(mun,x1, x2,x4, x5, x7, x8, x9,x12, x13, x15, x16, x17, x18)

df<-as.matrix(dados[,2:14])
row.names(df)<-dados$Municipio




# Matriz de correla��o

matcor<-cor(df)

corrplot(matcor, method="circle")

dev.copy(pdf,"Figuras/matriz_cor.pdf")
dev.off()


# Teste de Bartlett

cortest.bartlett(df)

# KMO

kmo<-KMO(df)

tab_kmo<-kmo$MSAi

tab1<-rotate_df(data.frame(tab_kmo), rn = c("Vari�vel"))
tab1[1]<-"KMO"


# Vari�ncia Explicada

tab2<-summary(princomp(df,cor=TRUE))

tab2<-data.frame(tab2[["sdev"]], 
                row.names = c(1:13))
colnames(tab2)<-c("Desvio Padr�o")

tab2<-tab2 %>% 
  mutate("Vari�ncia"=`Desvio Padr�o`^2) %>% 
  mutate("Propor��o"= Vari�ncia/sum(Vari�ncia))%>% 
   mutate("Acumulada"=cumsum(Propor��o)) %>% 
  select(-"Desvio Padr�o")



# Screeplot

var_tab2<-tab2$Vari�ncia

plot(var_tab2, lty=1, type="l",xlab = "Fator", ylab = "Vari�ncia", 
     pch=19, col="blue",xlim = c(1,8), lwd=2, bty="l")
abline(h = 1, col = "black", lty=2)
points(var_tab2,pch=18, cex=1.5)


dev.copy(pdf,"Figuras/screeplot.pdf")
dev.off()


# Rota��o

PCAvarimax<-principal(df, nfactors=3,
                           n.obs=92,rotate="varimax",scores=TRUE)



# Comunalidades

tab<-rotate_df(data.frame(PCAvarimax[["communality"]]), rn = c("Vari�vel"))
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

# Sa�das Latex

print(xtable(tab1, caption = "Teste de Kaiser-Meyer-Olkin", 
             label = "tab1"),
      caption.placement = "top", 
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))

print(xtable(tab2, caption = "Sum�rio do Total de Vari�ncia Explicada", 
             label = "tab2", digits =3 ),
      caption.placement = "top",
      include.rownames = TRUE,
      format.args = list(big.mark = ".", decimal.mark = ","))


print(xtable(tab, caption = "Comunalidades", 
             label = "tab", digits =2 ),
      caption.placement = "top",
      include.rownames = FALSE,
      format.args = list(big.mark = ".", decimal.mark = ","))




