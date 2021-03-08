# Carregando pacotes a serem utilizados


library(foreign)
library(dplyr)
library(ggplot2)
library(factoextra)
library(NbClust)

# Limpar----

rm(list=ls())


#Diretório

setwd("c:/Users/CAIO AZEVEDO/Documents/Documentos Caio/Github/Monografia")

# Importando os dados disponíveis no GitHub

site<-"https://raw.githubusercontent.com/caio-azevedo/Monografia/master/data/base_fatorial.csv"
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
            max.nc = 9, index = "ch")

ch_single<-data.frame(ch_single$All.index)
colnames(ch_single)<-c("Single")


# Average

ch_average<-NbClust(df, distance = "euclidean", method = "average", 
            max.nc = 9, index = "ch")

ch_average<-data.frame(ch_average$All.index)
colnames(ch_average)<-c("Average")


# Complete

ch_complete<-NbClust(df, distance = "euclidean", method = "complete", 
                    max.nc = 9, index = "ch")

ch_complete<-data.frame(ch_complete$All.index)
colnames(ch_complete)<-c("Complete")

# Ward

ch_ward<-NbClust(df, distance = "euclidean", method = "ward.D2", 
                     max.nc = 9, index = "ch")

ch_ward<-data.frame(ch_ward$All.index)
colnames(ch_ward)<-c("Ward")

# Tabela com as Estatísticas Pseudo F

pseudoF<-cbind(ch_single, ch_average, ch_complete, ch_ward)


# Duda----

# Single

duda_single<-NbClust(df, distance = "euclidean", method = c("single"), 
                   max.nc = 9, index = "duda")

duda_single<-data.frame(duda_single$All.index)
colnames(duda_single)<-c("Single")


# Average

duda_average<-NbClust(df, distance = "euclidean", method = "average", 
                    max.nc = 9, index = "duda")

duda_average<-data.frame(duda_average$All.index)
colnames(duda_average)<-c("Average")


# Complete

duda_complete<-NbClust(df, distance = "euclidean", method = "complete", 
                     max.nc = 9, index = "duda")

duda_complete<-data.frame(duda_complete$All.index)
colnames(duda_complete)<-c("Complete")

# Ward

duda_ward<-NbClust(df, distance = "euclidean", method = "ward.D2", 
                 max.nc = 9, index = "duda")

duda_ward<-data.frame(duda_ward$All.index)
colnames(duda_ward)<-c("Ward")

# Tabela com as Estatísticas Duda

DUDA<-cbind(duda_single, duda_average, duda_complete, duda_ward)

# Hart----

# Single

hart_single<-NbClust(df, distance = "euclidean", method = c("single"), 
                     max.nc = 9, index = "pseudot2")

hart_single<-data.frame(hart_single$All.index)
colnames(hart_single)<-c("Single")


# Average

hart_average<-NbClust(df, distance = "euclidean", method = "average", 
                      max.nc = 9, index = "pseudot2")

hart_average<-data.frame(hart_average$All.index)
colnames(hart_average)<-c("Average")


# Complete

hart_complete<-NbClust(df, distance = "euclidean", method = "complete", 
                       max.nc = 9, index = "pseudot2")

hart_complete<-data.frame(hart_complete$All.index)
colnames(hart_complete)<-c("Complete")

# Ward

hart_ward<-NbClust(df, distance = "euclidean", method = "ward.D2", 
                   max.nc = 9, index = "pseudot2")

hart_ward<-data.frame(hart_ward$All.index)
colnames(hart_ward)<-c("Ward")

# Tabela com as Estatísticas Duda

HART<-cbind(hart_single, hart_average, hart_complete, hart_ward)


# Dendograma

fviz_dend(average,rect=TRUE,k=9,cex = 0.8, main="", ylab = "Altura")


# Partição final

part=9 # definir o número de clusters para a partição final

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
  summarise(Media_1=mean(RC1), Media_2=mean(RC2), Media_3=mean(RC3), Freq=n())


# Conferir

library(cluster)
library(clValid)

valida<-clValid(as.data.frame(df),6,clMethods = c("hierarchical","kmeans"), 
                validation = "internal", metric="euclidean",method="complete")

summary(valida)


estabilidade<-clValid(as.data.frame(df),6,clMethods = c("hierarchical"), 
                      validation = "stability", metric="euclidean",
                      method=c("complete"))

summary(estabilidade)

estab_kmeans<-clValid(as.data.frame(df),6,clMethods = c("kmeans"), 
                      validation = "stability")

summary(estab_kmeans)

