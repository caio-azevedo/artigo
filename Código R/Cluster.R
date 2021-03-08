# Carregando pacotes a serem utilizados


library(foreign)
library(dplyr)
library(ggplot2)
library(factoextra)
library(NbClust)
library(xtable)

# Limpar----

rm(list=ls())


#Diretório

setwd("c:/Users/CAIO AZEVEDO/Documents/Documentos Caio/Github/artigo")

# Importando os dados disponíveis no GitHub

site<-"https://raw.githubusercontent.com/caio-azevedo/artigo/main/data/base_fatorial.csv"
dados<- read.table(site, header=T, sep=";")


# Preparação dos dados

df<-as.matrix(dados)



# Medidas de dissimilaridade----

# Distância Euclidiana

dist_euc<-dist(df, method = "euclidean")



# Métodos para construção de Clusters

# Técnicas Hierárquicas

single<-hclust(dist_euc, method = "single")
average<-hclust(dist_euc,method = "average")
complete<-hclust(dist_euc, method = "complete")
ward<-hclust(dist_euc, method="ward.D2")


# Calinski e Harabasz----

# Single

ch_single<-NbClust(df, distance = "euclidean", method = c("single"), 
            max.nc = 12, index = "ch")

ch_single<-data.frame(ch_single$All.index)
colnames(ch_single)<-c("Single")


# Average

ch_average<-NbClust(df, distance = "euclidean", method = "average", 
            max.nc = 12, index = "ch")

ch_average<-data.frame(ch_average$All.index)
colnames(ch_average)<-c("Average")


# Complete

ch_complete<-NbClust(df, distance = "euclidean", method = "complete", 
                    max.nc = 12, index = "ch")

ch_complete<-data.frame(ch_complete$All.index)
colnames(ch_complete)<-c("Complete")

# Ward

ch_ward<-NbClust(df, distance = "euclidean", method = "ward.D2", 
                     max.nc = 12, index = "ch")

ch_ward<-data.frame(ch_ward$All.index)
colnames(ch_ward)<-c("Ward")

# Tabela com as Estatísticas Pseudo F

pseudoF<-cbind(ch_single, ch_average, ch_complete, ch_ward)


# Duda----

# Single

duda_single<-NbClust(df, distance = "euclidean", method = c("single"), 
                   max.nc = 12, index = "duda")

duda_single<-data.frame(duda_single$All.index)
colnames(duda_single)<-c("Single")


# Average

duda_average<-NbClust(df, distance = "euclidean", method = "average", 
                    max.nc = 12, index = "duda")

duda_average<-data.frame(duda_average$All.index)
colnames(duda_average)<-c("Average")


# Complete

duda_complete<-NbClust(df, distance = "euclidean", method = "complete", 
                     max.nc = 12, index = "duda")

duda_complete<-data.frame(duda_complete$All.index)
colnames(duda_complete)<-c("Complete")

# Ward

duda_ward<-NbClust(df, distance = "euclidean", method = "ward.D2", 
                 max.nc = 12, index = "duda")

duda_ward<-data.frame(duda_ward$All.index)
colnames(duda_ward)<-c("Ward")

# Tabela com as Estatísticas Duda

DUDA<-cbind(duda_single, duda_average, duda_complete, duda_ward)

# Hart----

# Single

hart_single<-NbClust(df, distance = "euclidean", method = c("single"), 
                     max.nc = 12, index = "pseudot2")

hart_single<-data.frame(hart_single$All.index)
colnames(hart_single)<-c("Single")


# Average

hart_average<-NbClust(df, distance = "euclidean", method = "average", 
                      max.nc = 12, index = "pseudot2")

hart_average<-data.frame(hart_average$All.index)
colnames(hart_average)<-c("Average")


# Complete

hart_complete<-NbClust(df, distance = "euclidean", method = "complete", 
                       max.nc = 12, index = "pseudot2")

hart_complete<-data.frame(hart_complete$All.index)
colnames(hart_complete)<-c("Complete")

# Ward

hart_ward<-NbClust(df, distance = "euclidean", method = "ward.D2", 
                   max.nc = 12, index = "pseudot2")

hart_ward<-data.frame(hart_ward$All.index)
colnames(hart_ward)<-c("Ward")

# Tabela com as Estatísticas Duda

HART<-cbind(hart_single, hart_average, hart_complete, hart_ward)


# Dendograma

fviz_dend(ward,rect=T,k=7, main="", ylab = "Altura", 
          show_labels = F, rect_border = "black")

dev.copy(pdf,"Figuras/dendograma.pdf")
dev.off()



# Partição final

part=7 # definir o número de clusters para a partição final

cluster_single<-as.data.frame(cutree(single, k=part))
colnames(cluster_single)<-c("Single")

cluster_average<-as.data.frame(cutree(average, k=part))
colnames(cluster_average)<-c("Average")

cluster_complete<-as.data.frame(cutree(complete, k=part))
colnames(cluster_complete)<-c("Complete")

cluster_ward<-as.data.frame(cutree(ward, k=part))
colnames(cluster_ward)<-c("Ward")

# Base de dados com as partições finais

base<-cbind(df, cluster_single, cluster_average, cluster_complete, cluster_ward)

# Sumário dos clusters

sumario<-base %>% 
  group_by(Ward) %>% 
  summarise(Freq=n(), 
            Media_1=mean(RC1), Var_1=var(RC1), Min_1=min(RC1), Max_1=max(RC1),
            Media_2=mean(RC2), Var_2=var(RC2), Min_2=min(RC2), Max_2=max(RC2), 
            Media_3=mean(RC3), Var_3=var(RC3), Min_3=min(RC3), Max_3=max(RC3))

site2<-"https://raw.githubusercontent.com/caio-azevedo/artigo/main/data/base.csv"

mun<- read.table(site2, header=T, sep=";")
mun<-mun[,1]

base<-cbind(mun,base)

base<-base %>% 
  mutate("dif"=ifelse(Complete!=Ward,1,0)) %>% 
  arrange(Ward)

sum(base$dif)


# Conferir

library(cluster)
library(clValid)

valida<-clValid(as.data.frame(df),7,clMethods = c("hierarchical","kmeans"), 
                validation = "internal", metric="euclidean",method="complete")

summary(valida)

estabilidade<-clValid(as.data.frame(df),7,clMethods = c("hierarchical"), 
                      validation = "stability", metric="euclidean",
                      method=c("complete"))

summary(estabilidade)

estab_kmeans<-clValid(as.data.frame(df),7,clMethods = c("kmeans"), 
                      validation = "stability")

summary(estab_kmeans)


# Saídas Latex

pseudoF<-data.frame(pseudoF)

pseudoF<-pseudoF %>% 
  mutate("i"=seq(1,11,1)) %>% 
  mutate("s"=ifelse(i>1,(Single[i]-lag(Single,1))/lag(Single,1),0))%>% 
  mutate("a"=ifelse(i>1,(Average[i]-lag(Average,1))/lag(Average,1),0))%>%
  mutate("c"=ifelse(i>1,(Complete[i]-lag(Complete,1))/lag(Complete,1),0))%>% 
  mutate("w"=ifelse(i>1,(Ward[i]-lag(Ward,1))/lag(Ward,1),0)) %>% 
  mutate("média"=ifelse(i>1, (s[i]+a[i]+c[i]+w[i])/4,0)) %>% 
  mutate("s_1"=ifelse(s[i]>0,1,0)) %>% 
  mutate("a_1"=ifelse(a[i]>0,1,0)) %>%
  mutate("c_1"=ifelse(c[i]>0,1,0)) %>% 
  mutate("w_1"=ifelse(w[i]>0,1,0)) %>% 
  mutate("pos"=s_1[i]+a_1[i]+c_1[i]+w_1[i]) %>% 
  select(Single, Average, Complete, Ward, média, pos)
  

hart<-data.frame(HART)

hart<-hart %>% 
  mutate("i"=seq(1,11,1)) %>% 
  mutate("s"=ifelse(i>1,(Single[i]-lag(Single,1))/lag(Single,1),0))%>% 
  mutate("a"=ifelse(i>1,(Average[i]-lag(Average,1))/lag(Average,1),0))%>%
  mutate("c"=ifelse(i>1,(Complete[i]-lag(Complete,1))/lag(Complete,1),0))%>% 
  mutate("w"=ifelse(i>1,(Ward[i]-lag(Ward,1))/lag(Ward,1),0)) %>% 
  mutate("média"=ifelse(i>1, (s[i]+a[i]+c[i]+w[i])/4,0)) %>% 
  mutate("s_1"=ifelse(s[i]<0,1,0)) %>% 
  mutate("a_1"=ifelse(a[i]<0,1,0)) %>%
  mutate("c_1"=ifelse(c[i]<0,1,0)) %>% 
  mutate("w_1"=ifelse(w[i]<0,1,0)) %>% 
  mutate("pos"=s_1[i]+a_1[i]+c_1[i]+w_1[i]) %>% 
  select(Single, Average, Complete, Ward, média, pos)


pseudoF_T<-cbind(pseudoF,hart)


print(xtable(pseudoF_T, caption = "Testes para a definição do número ótimo de clusters", 
             label = "pseudo-f", digits =2 ),
      caption.placement = "top",
      include.rownames = TRUE,
      format.args = list(big.mark = ".", decimal.mark = ","))


print(xtable(sumario, caption = "sumario", 
             label = "sumario", digits =2 ),
      caption.placement = "top",
      include.rownames = F,
      format.args = list(big.mark = ".", decimal.mark = ","))







