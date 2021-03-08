# Carregando pacotes a serem utilizados

library(dplyr)
library(readxl)
library(stringr)
library(sjlabelled)



# Limpar----

rm(list=ls())


#Diretório

setwd("c:/Users/CAIO AZEVEDO/Documents/Documentos Caio/Github/artigo")

# Preparação dos dados

# Produto Interno Bruto----

pib<- read_xlsx("data/tabela21.xlsx", sheet = "Tabela 1", col_names = c("mun", "pib"))

pib<-pib[-c(1:5,98),]

# Valor Adicionado bruto a preços correntes----

vab<- read_xlsx("data/tabela21.xlsx", sheet = "Tabela 2", col_names = c("mun", "vab"))

vab<-vab[-c(1:5,98),]

# Valor adicionado bruto a preços correntes da agropecuária----

agro<- read_xlsx("data/tabela21.xlsx", sheet = "Tabela 4", col_names = c("mun", "agro"))

agro<-agro[-c(1:5,98),]

# Valor adicionado bruto a preços correntes da indústria----

ind<- read_xlsx("data/tabela21.xlsx", sheet = "Tabela 6", col_names = c("mun", "ind"))

ind<-ind[-c(1:5,98),]

# Valor adicionado bruto a preços correntes dos serviços, inclusive administração, defesa, educação e saúde públicas e seguridade social

serv<- read_xlsx("data/tabela21.xlsx", sheet = "Tabela 8", col_names = c("mun", "serv"))

serv<-serv[-c(1:5,98),]

# Juntando as tabelas

valor_adicionado<-full_join(vab, agro,c("mun"))
valor_adicionado<-full_join(valor_adicionado, ind,c("mun"))
valor_adicionado<-full_join(valor_adicionado, serv,c("mun"))

rm(vab, agro, ind, serv)


# Criando as variáveis relativas

valor_adicionado<-valor_adicionado %>% 
  mutate("vab"=as.numeric(vab)) %>% 
  mutate("agro"=as.numeric(agro)) %>% 
  mutate("ind"=as.numeric(ind)) %>% 
  mutate("serv"=as.numeric(serv)) %>% 
  mutate(x1=agro/vab) %>% 
  mutate(x2=ind/vab) %>% 
  mutate(x3=serv/vab) %>% 
  select(mun,x1,x2,x3)

# Pessoas ocupadas no comércio----

ocup<- read_xlsx("data/tabela3594.xlsx", 
                 col_names = c("mun","total","transf", "extrat","comercio"),
                 col_types = c("text","numeric","numeric","numeric","numeric"),
                 skip=7, n_max = 92)

com<-ocup %>%
  mutate(x4=comercio/total) %>% 
  select(mun,x4)


# Juntando a base existente

dados<-full_join(valor_adicionado, com, c("mun"))

rm(com,valor_adicionado)

#  Taxa de alfabetização das pessoas de 10 anos ou mais de idade (%)----

alf<- read_xlsx("data/tabela1383.xlsx", col_names = c("mun", "x5"))

alf<-alf[-c(1:6,99),]

# Juntando as variáveis x1:x5

dados<-full_join(dados,alf, c("mun"))

rm(alf)


# População residente----

urb<- read_xlsx("data/tabela608.xlsx", col_names = c("mun", "sexo", "total", "urbana"))

urb<-urb[-c(1:5,98),-2]

pop<-urb[,1:2]

colnames(pop)<-c("mun","pop")

pop_mun<-toupper(pop$mun)

pop_mun<-chartr("ÁÉÍÓÚÃÕÂÊÔÇ", "AEIOUAOAEOC", pop_mun)

pop<-cbind(pop_mun,pop)
pop<-pop[,-2]
colnames(pop)<-c("mun", "pop")

rm(pop_mun)

urb<-urb %>% 
  mutate("total"=as.numeric(total)) %>% 
  mutate("urbana"=as.numeric(urbana)) %>% 
  mutate(x7=urbana/total) %>% 
  select(mun,x7)



# Frota de veículos----

veic<- read_xls("data/Frota Munic DEZ2010.xls", sheet = "DEZ_2010")

colnames(veic)<-c(veic[2,])
veic<-veic[-c(1:2),]

# Filtrando pelo Estado

veic<-veic %>% 
  filter(UF=="RJ") %>% 
  select(MUNICIPIO, TOTAL)


# Inserindo (RJ) para junção das tabelas

mun<-paste(veic$MUNICIPIO, "(RJ)", sep=" ")
veic<-cbind(mun,veic)
veic<-veic[,-2]

# Corrigindo ortagrafia da base 

veic[5,1]<-c("ARMACAO DOS BUZIOS (RJ)")
veic[53,1]<-c("PARATY (RJ)")
veic[87,1]<-c("TRAJANO DE MORAES (RJ)")

rm(mun)

# Juntando as variáveis x6, x7

veic<-left_join(pop, veic, c("mun") )


veic<-veic %>%
  mutate("TOTAL"=as.numeric(TOTAL)) %>% 
  mutate("pop"=as.numeric(pop)) %>% 
  mutate(x6=TOTAL/pop) %>% 
  select("mun", "x6")



# Juntando x6 -x7

x<-cbind(veic,urb)
x<-x[,-1]
rm(veic, urb)


# Juntando a base existente

dados<-full_join(dados, x, c("mun"))

rm(x)


# Rendimento nominal mensal----
rend<- read_xlsx("data/tabela3548.xlsx", 
                col_names = c("mun","tot1", "tot2", "tot3", "x8"), 
                sheet = "Valor do rendimento nominal ...",
                skip=7, n_max = 92)

rend<-rend %>% 
  select(mun,x8)

# Juntando a base existente
dados<-full_join(dados, rend, c("mun"))

rm(rend)



# Operações de crédito----

banco<- read.csv("data/201012_ESTBAN.csv",sep = ";", header = T, skip=2)

banco<-banco %>% 
  select("UF","MUNICIPIO","AGEN_PROCESSADAS","VERBETE_160_OPERACOES_DE_CREDITO") %>% 
  filter(UF=="RJ") %>% 
  select(-UF) %>% 
  na.exclude()

banco<-banco %>% 
  group_by(MUNICIPIO) %>% 
  summarise(x9=sum(VERBETE_160_OPERACOES_DE_CREDITO))

banco$MUNICIPIO[53]<-c("PARATY")
banco$MUNICIPIO[87]<-c("TRAJANO DE MORAES")

# Inserindo (RJ) para junção das tabelas

MUN<-paste(banco$MUNICIPIO, "(RJ)", sep=" ")

banco<-cbind(MUN,banco)

banco<-banco %>% 
  select(-MUNICIPIO)

pop1<-pop %>% 
  rename("MUN"=mun)
  
banco<-full_join(banco,pop1,c("MUN"))

banco<-banco %>% 
  mutate(pop=as.numeric(pop)) %>% 
  mutate(x9=x9/pop) %>% 
  select("MUN","x9")

rm(pop1)

MUN<-toupper(dados$mun)
MUN<-chartr("ÁÉÍÓÚÃÕÂÊÔÇ", "AEIOUAOAEOC", MUN)


dados<-cbind(MUN,dados)

dados<-full_join(dados, banco, c("MUN"))

dados<-dados %>% 
  select(-"MUN")

rm(MUN,banco)


# Leitos por mil habitantes----

leitos<- read.csv("data/A182143189_28_143_208.csv",sep = ";", skip = 4)

mun<-str_sub(leitos$Município,start=8)
mun<-paste(mun, "(RJ)", sep=" ")

leitos<-cbind(mun,leitos)
leitos<-leitos[-c(85:96),-2]
rm(mun)

urb<- read_xlsx("data/tabela608.xlsx", col_names = c("mun", "sexo", "total", "urbana"))

urb<-urb[-c(1:5,98),-2]

pop<-urb[,1:2]

colnames(pop)<-c("mun","pop")

leitos<-full_join(pop, leitos, c("mun"))

leitos<-leitos %>% 
  mutate("pop"=as.numeric(pop)) %>% 
  mutate("Quantidade_existente"=as.numeric(Quantidade_existente)) %>% 
  mutate(leitos=(Quantidade_existente/pop)*1000) %>% 
  rename("x10"=leitos) %>% 
  select("mun", "x10")

# Juntando a base existente

dados<-full_join(dados, leitos, c("mun"))

rm(leitos,urb)

# Domicilios com ordenamento regular adequado----

dom<- read_xlsx("data/tabela3362.xlsx", 
                col_names = c("mun", "exist", "total", "adequado"), skip=6, n_max=92)

dom<-dom %>% 
  select(-"exist") %>% 
  mutate(x11=adequado/total) %>% 
  select(mun,x11)

# Juntando a base existente

dados<-full_join(dados, dom, c("mun"))

rm(dom)


#IDHM----

idhm<- read_xlsx("data/atlas2013_dadosbrutos_pt.xlsx", 
                 sheet = "MUN 91-00-10")

idhm<-idhm %>% 
  filter(ANO==2010,UF==33) %>% 
  select(Município,IDHM) %>% 
  rename("x12"=IDHM)

MUN<-paste(idhm$Município, "(RJ)", sep=" ")
idhm<-cbind(MUN,idhm)
idhm<-idhm[,-2]

rm(MUN)

# Juntando a base existente ----

MUN<-toupper(dados$mun)
dados<-cbind(MUN,dados)

dados<-full_join(dados, idhm, c("MUN"))

dados<-dados %>% 
  select(-MUN)

rm(idhm)

#PIB per capita----

pib_per<-full_join(pib,pop,c("mun"))

pib_per<-pib_per %>%
  mutate(pib=as.numeric(pib)) %>% 
  mutate(pop=as.numeric(pop)) %>% 
  mutate(x13=pib/pop) %>% 
  select(-pib,-pop)


# Juntando a base existente

dados<-full_join(dados, pib_per, c("mun"))

rm(pib_per,pib,pop)



# Pessoas ocupadas na indústria----

ind<-ocup %>%
  mutate(x14=(transf+extrat)/total) %>% 
  select(mun,x14)


# Juntando a base existente

dados<-full_join(dados, ind, c("mun"))

rm(ind,ocup)


#Domicilios com microcomputador e internet----

comp<- read_xlsx("data/tabela2249.xlsx", 
                  col_names = c("mun","total", "pc"), 
                  skip=5, n_max = 92)

comp<-comp %>% 
  mutate(x15=pc/total) %>% 
  select(mun,x15)

# Juntando a base existente

dados<-full_join(dados, comp, c("mun"))

rm(comp)

#Domicilios com rendimento de 10 a 20 salarios----

rend10<- read_xlsx("data/tabela3571.xlsx", 
                 col_names = c("mun","total", "rend"), 
                 skip=6, n_max = 92)

rend10<-rend10 %>% 
  mutate(x16=rend/total) %>% 
  select(mun,x16)

# Juntando a base existente

dados<-full_join(dados, rend10, c("mun"))

rm(rend10)

#Domicilios tipo apartamento----

apart<- read_xlsx("data/tabela3513.xlsx", 
                col_names = c("mun","tipo", "exist", "total", "apartamento"), 
                skip=7, na="-", n_max = 92)

apart<-apart %>% 
  select(-"tipo",-"exist") %>% 
  mutate(x17=apartamento/total) %>% 
  select(mun,x17)

# Juntando a base existente

dados<-full_join(dados, apart, c("mun"))

rm(apart)

# Bolsa família----

bolsa_fam<- read.csv("data/visdata3-download-05-03-2021 143251.csv",sep = ",", 
                      col.names = c("cod","mun","uf","ref","qtd"))

# Inserindo (RJ) para junção das tabelas

MUN<-paste(bolsa_fam$mun, "(RJ)", sep=" ")

bolsa_fam<-cbind(MUN,bolsa_fam)

bolsa_fam<-bolsa_fam %>% 
  select("MUN","qtd") %>% 
  rename(x18=qtd)


#Adicionando domicílios

dom<- read_xlsx("data/tabela1134.xlsx", 
                  col_names = c("mun","tot1", "tot2", "tot3", "domicilios"), 
                  skip=7, n_max = 92)

dom<-dom %>% 
  select(mun,domicilios)

rm(MUN)

# Juntando a base existente
MUN<-toupper(dom$mun)
dom<-cbind(MUN,dom)

bolsa_fam<-full_join(dom, bolsa_fam, c("MUN"))

bolsa_fam<-bolsa_fam %>% 
    mutate(x18=x18/domicilios) %>% 
  select(-"MUN",-"domicilios") 

rm(MUN,dom)


# Juntando a base existente
dados<-full_join(dados, bolsa_fam, c("mun"))

rm(bolsa_fam)

# Trocando NA por zero

dados<- replace(dados, list = is.na(dados), values = 0)

#Exportando----
write.table(dados,file='data/base.csv',sep=';', row.names = F)
write_spss(dados, "base.sav")


