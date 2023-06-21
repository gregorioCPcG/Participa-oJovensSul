library(tidyverse)
rm(list=ls())
library(sjPlot)
library(wesanderson)
library(memisc)
library(huxtable)
library(lavaan)
library(semTools)
library(psych)
library(haven)
library(lavaanPlot)
library(readxl)
library(mice)
Poa <- read_excel("POA.xls")

# 
table(Poa$q19.9DemocraciaMelhorFormaGoverno)#usar - valores positivos antidemoc
Poa$Q19.9_recod_numeric <- ifelse(Poa$q19.9DemocraciaMelhorFormaGoverno %in% c(55,66, 88), NA, Poa$q19.9DemocraciaMelhorFormaGoverno)
table(Poa$Q19.9_recod_numeric)
table(Poa$q19.10AlgumasSituaçoesAutoritárioMelhor)# valores negativos antidemoc
Poa$Q19.10_recod_numeric <- ifelse(Poa$q19.10AlgumasSituaçoesAutoritárioMelhor %in% c(55,66, 88), NA, Poa$q19.10AlgumasSituaçoesAutoritárioMelhor)
table(Poa$Q19.10_recod_numeric)
Poa$Q19.10_recod_numeric <- memisc::recode(as.numeric(Poa$Q19.10_recod_numeric),
                                           1 <- c(3),
                                           2 <- c(2),
                                           3<-c(1))
table(Poa$Q19.10_recod_numeric)
cor.test(Poa$Q19.10_recod_numeric, Poa$Q19.9_recod_numeric)

Poa$Q19.10_recod_numeric <- scales::rescale(Poa$Q19.10_recod_numeric, to = c(0, 1))
Poa$Q19.9_recod_numeric <- scales::rescale(Poa$Q19.9_recod_numeric, to = c(0, 1))


table(Poa$q24.1ToleranciaReligioes)#3 é tolerante
Poa$q24.1_recod_numeric <- ifelse(Poa$q24.1ToleranciaReligioes %in% c(4,
                                                                      55,
                                                                      66,
                                                                      88),
                                  NA, Poa$q24.1ToleranciaReligioes)
table(Poa$q24.1_recod_numeric)
table(Poa$q24.4ToleranciaHomossexuais)#3 é tolerante
Poa$q24.4_recod_numeric <- ifelse(Poa$q24.4ToleranciaHomossexuais %in% c(4,
                                                                         55,
                                                                         66,
                                                                         88),
                                  NA, Poa$q24.4ToleranciaHomossexuais)
table(Poa$q24.4_recod_numeric)


table(Poa$q24.6ToleranciaOpinioesPoliticas)#3 é tolerante
Poa$q24.6_recod_numeric <- ifelse(Poa$q24.6ToleranciaOpinioesPoliticas %in% c(4,
                                                                              55,
                                                                              66,
                                                                              88),
                                  NA, Poa$q24.6ToleranciaOpinioesPoliticas)
table(Poa$q24.6_recod_numeric)



Poa$q24.4_recod_numeric <- memisc::recode(as.numeric(Poa$q24.4_recod_numeric),#inverter para valores negativos ser intolerante
                                          1 <- c(3),
                                          2 <- c(2),
                                          3<-c(1))
Poa$q24.1_recod_numeric <- memisc::recode(as.numeric(Poa$q24.1_recod_numeric),#inverter para valores negativos ser intolerante
                                          1 <- c(3),
                                          2 <- c(2),
                                          3<-c(1))
Poa$q24.6_recod_numeric <- memisc::recode(as.numeric(Poa$q24.6_recod_numeric),#inverter para valores negativos ser intolerante
                                          1 <- c(3),
                                          2 <- c(2),
                                          3<-c(1))

Poa$q24.6_recod_numeric <- scales::rescale(Poa$q24.6_recod_numeric, to = c(0, 1))
Poa$q24.1_recod_numeric <- scales::rescale(Poa$q24.1_recod_numeric, to = c(0, 1))
Poa$q24.4_recod_numeric <- scales::rescale(Poa$q24.4_recod_numeric, to = c(0, 1))
table(Poa$q24.1_recod_numeric)
table(Poa$q24.1ToleranciaReligioes)


Poa$q32.1ResponsavelIndustriasImportantes -> Poa$Q32.1_recod_numeric
Poa$q32.2ResponsavelBemEstarCidadaos -> Poa$Q32.2_recod_numeric
Poa$q32.3ResponsavelAposentadorias -> Poa$Q32.3_recod_numeric
Poa$q32.4ResponsavelSaúde -> Poa$Q32.4_recod_numeric
Poa$q32.5ResponsavelEducação -> Poa$Q32.5_recod_numeric

Poa$Q32.1_recod_numeric <- ifelse(Poa$Q32.1_recod_numeric > 2, NA, Poa$Q32.1_recod_numeric)
table(Poa$Q32.1_recod_numeric)
Poa$Q32.2_recod_numeric <- ifelse(Poa$Q32.2_recod_numeric > 2, NA, Poa$Q32.2_recod_numeric)
table(Poa$Q32.2_recod_numeric)
Poa$Q32.3_recod_numeric <- ifelse(Poa$Q32.3_recod_numeric > 2, NA, Poa$Q32.3_recod_numeric)
table(Poa$Q32.3_recod_numeric)
Poa$Q32.4_recod_numeric <- ifelse(Poa$Q32.4_recod_numeric > 2, NA, Poa$Q32.4_recod_numeric)
table(Poa$Q32.4_recod_numeric)
Poa$Q32.5_recod_numeric <- ifelse(Poa$Q32.5_recod_numeric > 2, NA, Poa$Q32.5_recod_numeric)
table(Poa$Q32.5_recod_numeric)

Poa$Q32.1_recod_numeric <- scales::rescale(Poa$Q32.1_recod_numeric, to = c(0, 1))
Poa$Q32.2_recod_numeric <- scales::rescale(Poa$Q32.2_recod_numeric, to = c(0, 1))
Poa$Q32.3_recod_numeric <- scales::rescale(Poa$Q32.3_recod_numeric, to = c(0, 1))
Poa$Q32.4_recod_numeric <- scales::rescale(Poa$Q32.4_recod_numeric, to = c(0, 1))
Poa$Q32.5_recod_numeric <- scales::rescale(Poa$Q32.5_recod_numeric, to = c(0, 1))



# CFA
Poa -> Poa_completo
Poa <- Poa[,257:266]
summary(Poa)


imp <- mice(Poa, seed=23109)#a seed sempre essa 23109
Poa <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(Poa)

scree(Poa)
nfactors(Poa)
fa2 <-psych::fa(Poa,2)
fa2$loadings
fa3 <-psych::fa(Poa,3)
fa3$loadings



modelo_cfa <- '
  Intolerância =~ q24.1_recod_numeric + q24.4_recod_numeric + q24.6_recod_numeric
  Visão_Privatista =~ Q32.2_recod_numeric + Q32.3_recod_numeric + Q32.4_recod_numeric + Q32.5_recod_numeric'
ajuste_cfa <- cfa(modelo_cfa, data = Poa,check.gradient = FALSE)

summary(ajuste_cfa, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
semTools::fitmeasures(ajuste_cfa, c("tli", "cfi", "rmsea"))


lavaanPlot(model =ajuste_cfa, node_options = list(shape = "circle", fontname = 
                                                    "Garamond"), edge_options = list(color = "blue"), coefs = T,covs=
             F,stars = TRUE)


scores <- lavPredict(ajuste_cfa)
Poa_completo -> df
scores[,1] -> df$Intolerancia 
scores[,2] -> df$Privatista 

df$TipoEscola
df$Tipo_Escola <- ifelse(df$TipoEscola == 1, "Pública", 
                         ifelse(df$TipoEscola == 2, "Privada", NA))
table(df$Tipo_Escola)#deu
df$Intolerancia <- scales::rescale(df$Intolerancia, to = c(0, 1))
df$Privatista <- scales::rescale(df$Privatista, to = c(0, 1))

hist(df$Intolerancia, breaks = 40, main="")
hist(df$Privatista, breaks =40, main = "")


table(df$q63aEtnia)
df$Etnia_Branco <- df$q63aEtnia ==3



df$Tipo_Escola <- as.factor(df$Tipo_Escola)#tipo escola
sum(is.na(df$Tipo_Escola))#2 aceitável ok
#classe:
table(df$q65ClasseSocial)#
# Criar nova variável Classe
df$Classe <- ifelse(df$q65ClasseSocial == 1, "Baixo(pobre)",
                    ifelse(df$q65ClasseSocial == 2, "Média Baixa",
                           ifelse(df$q65ClasseSocial == 3, "Média",
                                  ifelse(df$q65ClasseSocial %in% c(4,5), "Média Alta e Alta(rico)",
                                         ifelse(df$q65ClasseSocial %in% c(55,66,77,99), "NS/NR", NA)))))

# Transformar em fator
df$Classe <- factor(df$Classe, levels = c("Baixo(pobre)", "Média Baixa", "Média", "Média Alta e Alta(rico)", "NS/NR"))

# Definir categoria referência
df$Classe <- relevel(df$Classe, ref = "Baixo(pobre)")
table(df$Classe)#ok!
sum(is.na(df$Classe))
df$Classe[is.na(df$Classe)] <- "NS/NR"
table(df$Classe)#ok!
sum(is.na(df$Classe))#OK!

#religiosidade :
table(df$q35.3ParticipaAtividadesReligiosas)#proxy de religiosidade
sum(is.na(df$q35.3ParticipaAtividadesReligiosas))
# 1 muito religioso
# 2 um pouoc
# 3 nada
df$religiosidade_numeric <- ifelse(df$q35.3ParticipaAtividadesReligiosas > 3, NA, df$q35.3ParticipaAtividadesReligiosas)
mean(df$religiosidade_numeric, na.rm = TRUE)
#imputar média para NA
table(df$religiosidade_numeric)
summary(df$religiosidade_numeric)
df$religiosidade_numeric[is.na(df$religiosidade_numeric)] <- 2.340828#medida abandonada em versões mais finais
# agora falta inverter para valores maiores serem religiosidade
df$religiosidade_bin <- df$q35.3ParticipaAtividadesReligiosas == 1
table(df$religiosidade_bin)


#escolaridade dos pais:
table(df$q59.1EscolaridadeMãe)
table(df$q59.2EscolaridadePai)
df$escolMae_num <- ifelse(df$q59.1EscolaridadeMãe > 7, NA, df$q59.1EscolaridadeMãe)
table(df$escolMae_num)
df$escolPai_num <- ifelse(df$q59.2EscolaridadePai > 7, NA, df$q59.2EscolaridadePai)
summary(df[,274:275])#verificar se pega 'escolPai_num' e 'escolMae_num"

imp <- mice(df[,274:275], seed=23109)#a seed sempre essa 23109
df[,274:275] <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(df[,274:275])


df$escolaridadePais <- df$escolMae_num + df$escolPai_num
table(df$escolaridadePais)
sum(is.na(df$escolaridadePais))#OK!


summary(df$Intolerancia)#OK
summary(df$Privatista)#Ok


# Genero
table(df$q2Sexo)
sum(is.na(df$q2Sexo))
df$q2Sexo[is.na(df$q2Sexo)] <- 66
df$Genero <- factor(ifelse(df$q2Sexo == 1, "MASC", 
                           ifelse(df$q2Sexo == 2, "FEM", NA)), 
                    levels = c("MASC", "FEM"))

table(df$Genero)
sum(is.na(df$Genero))
# as deps

# oque o cara (Dolezal,2022) fez:

#1. electoral (turnout in national elections), 
#2. partisan (working for a party or action group), 
#3. protest (taking part in legal demonstrations), 
#4. consumerism (boycotting certain products), and 
#5. online (posting or sharing anything about politics online). 
#6. an additive index of participation based on the sum of activities will be used as a dependent variable 

#proxys possíveis # binarizar todas (menos a 6)

#1 = electoral = q21VotariaSeVotoNãoObrigatório


#2 = partisan =  q35.1ParticipaPartidosPolíticos


#3 = protest =  q35.6ParticipaManifestações (geral)


#4 = consumerismo = Não deu

#5 = online = q35.10ParticipaMobilizaçõesRedesSocias


# 6 index = somar e reescalar de 0 a 1 (numeric)


# 1- voto

table(df$q21VotariaSeVotoNãoObrigatório)#NA considerar
df$Votaria <- ifelse(df$q21VotariaSeVotoNãoObrigatório > 2, NA, df$q21VotariaSeVotoNãoObrigatório)
table(df$Votaria)#inverter e lógica 1x0
df$Votaria <- memisc::recode(as.numeric(df$Votaria),
                             1 <- c(1),
                             0 <- c(2))
table(df$Votaria)#Ok 536


# 2- partisan
table(df$q35.1ParticipaPartidosPolíticos)#NA considerar
df$Partisan <- ifelse(df$q35.1ParticipaPartidosPolíticos > 3, NA,
                      df$q35.1ParticipaPartidosPolíticos)
table(df$Partisan)
#poucos casos - binarizar 1 e 2 como 1 e 3 como zero
df$Partisan <- memisc::recode(as.numeric(df$Partisan),
                              1 <- c(1,2),
                              0 <- c(3))
table(df$Partisan)#Ok 39


#3 - protest (proxy)
table(df$q35.6ParticipaManifestações)#NA considerar
df$Protest <- ifelse(df$q35.6ParticipaManifestações > 3, NA,
                     df$q35.6ParticipaManifestações)
table(df$Protest)
#poucos casos - binarizar 1 e 2 como 1 e 3 como zero
df$Protest <- memisc::recode(as.numeric(df$Protest),
                             1 <- c(1,2),
                             0 <- c(3))
table(df$Protest)#Ok 325


#4 nao tem

#5 online
table(df$q35.10ParticipaMobilizaçõesRedesSocias)#NA considerar
df$OnLine <- ifelse(df$q35.10ParticipaMobilizaçõesRedesSocias > 3, NA,
                    df$q35.10ParticipaMobilizaçõesRedesSocias)
table(df$OnLine)
#poucos casos - binarizar 1 e 2 como 1 e 3 como zero
df$OnLine <- memisc::recode(as.numeric(df$OnLine),
                            1 <- c(1,2),
                            0 <- c(3))
table(df$OnLine)#Ok 321




summary(df[,277:281])


sum(is.na(df$Votaria))
sum(is.na(df$Partisan))
sum(is.na(df$Protest))
sum(is.na(df$OnLine))

df$fem <- df$Genero == "FEM"
df$fem <- as.numeric(df$fem)
summary(df$fem)
df$escolapublica <- df$Tipo_Escola == "Pública"
df$escolapublica <- as.numeric(df$escolapublica)
summary(df$escolapublica)
sum(is.na(df$Tipo_Escola))
sd(df$fem, na.rm=T)
sd(df$escolapublica, na.rm=T)

prop.table(table(df$Classe))*100



# modelos 1 a 4 


modelo_Particip1Voto<- glm(Votaria ~ Genero+Tipo_Escola+Classe+
                             religiosidade_bin+Etnia_Branco+
                             escolaridadePais+Intolerancia+Privatista,
                           data=df,
                           family=binomial(link=logit))

modelo_Particip2Partisan <- glm(Partisan ~ Genero+Tipo_Escola+Classe+
                                  religiosidade_bin+Etnia_Branco+
                                  escolaridadePais+Intolerancia+Privatista,
                                data=df,
                                family=binomial(link=logit))
modelo_Particip3Protest <- glm(Protest ~ Genero+Tipo_Escola+Classe+
                                 religiosidade_bin+Etnia_Branco+
                                 escolaridadePais+Intolerancia+Privatista,
                               data=df,
                               family=binomial(link=logit))
modelo_Particip5OnLine <- glm(OnLine ~ Genero+Tipo_Escola+Classe+
                                religiosidade_bin+Etnia_Branco+
                                escolaridadePais+Intolerancia+Privatista,
                              data=df,
                              family=binomial(link=logit))

#6 index
#criar index participação
str(df[,278:281])#verificar se contém as 4
imp <- mice(df[,278:281], seed=23109)#a seed sempre essa 23109
df[,278:281] <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(df[,278:281])#pouca alteração

df$IndexParticipacao <- df$OnLine + df$Protest + df$Votaria + df$Partisan
hist(df$IndexParticipacao,breaks=50)#Ok
df$religiosidade_numeric <- as.numeric(df$religiosidade_bin)
df$etniaBranco_numeric <- as.numeric(df$Etnia_Branco)
summary(df[,267:285])#usar para tabela 1

sd(df$escolaridadePais)
sd(df$IndexParticipacao)


# modelo 6
modelo_Particip6index <- lm(IndexParticipacao ~ Genero+Tipo_Escola+Classe+
                              religiosidade_bin+Etnia_Branco+
                              escolaridadePais+Intolerancia+Privatista,
                            data=df)

# gráficos

tab_model(modelo_Particip1Voto, modelo_Particip2Partisan,modelo_Particip3Protest
          ,modelo_Particip5OnLine,modelo_Particip6index,
          show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")
library(marginaleffects)
predictions(modelo_Particip6index, newdata = datagrid(Genero = "FEM"))
predictions(modelo_Particip6index, newdata = datagrid(Genero = "MASC"))

options(scipen = 999)
# Visualizar os p-values do modelo
summary(modelo_Particip6index)$coefficients[, "Pr(>|t|)"]
# Visualizar os p-values do modelo logit
summary(modelo_Particip1Voto)$coefficients[, "Pr(>|z|)"]
summary(modelo_Particip2Partisan)$coefficients[, "Pr(>|z|)"]
summary(modelo_Particip3Protest)$coefficients[, "Pr(>|z|)"]
summary(modelo_Particip5OnLine)$coefficients[, "Pr(>|z|)"]

#grid arrange para todos os que deram sig
a<-plot_cap(modelo_Particip2Partisan, condition=c("Classe"),conf_level = .9)
b<-plot_cap(modelo_Particip5OnLine, condition=c("Classe"),conf_level = .9)
a<-a + theme_bw()
b<-b + theme_bw()
c <-plot_cap(modelo_Particip3Protest, condition=c("escolaridadePais"),conf_level = .9) 
c<-c + theme_bw()
a <- a + labs(y="Partidarismo")
a 
b 
c <- c + labs(y= "Protesto / Mobilização",
              x = "Escolaridade dos País (índice)")
c

d<-plot_cap(modelo_Particip3Protest, condition=c("Intolerancia"),conf_level = .9)
d <- d + theme_bw() + labs(y= "Protesto / Mobilização")
d

e<-plot_cap(modelo_Particip3Protest, condition=c("Privatista"),conf_level = .9)
e <- e + theme_bw()+ labs(y= "Protesto / Mobilização")
e

f<-plot_cap(modelo_Particip2Partisan, condition=c("Intolerancia"),conf_level = .9)
f <- f + theme_bw() + labs(y= "Partidarismo")
f





library(gridExtra)
library(grid)


# Criação dos gráficos (a, b, c, d, e, f)

# Define os nomes dos gráficos
nomes_graficos <- c("a", "b", "c", "d", "e", "f")

# Criação dos textos com os nomes dos gráficos
textos <- lapply(nomes_graficos, function(nome) {
  textGrob(nome, gp = gpar(fontsize = 12, fontface = "bold"))
})

# Organiza os gráficos e textos em uma única grade
grid.arrange(a, b, c, d, e, f, top = textos)

grid.arrange(a,b,c,d,e,f)





