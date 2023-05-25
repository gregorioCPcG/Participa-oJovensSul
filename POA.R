# 3st attempt - audio do Julian
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
Ctba <- read_sav("BancoCuritiba2022CORRIGIDO.sav")
library(readxl)
library(mice)
Poa <- read_excel("POA.xls")

# primeiro POA 2019
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

# Separando os valores de df$Intolerante para cada categoria de df$Tipo_Escola
x <- df$Intolerancia[df$TipoEscola == 1] # Intolerante para escolas públicas
y <- df$Intolerancia[df$TipoEscola == 2] # Intolerante para escolas privadas

# Executando o teste t de Student
t.test(x, y)


# Separando os valores de df$Intolerante para cada categoria de df$Tipo_Escola
x <- df$Privatista[df$TipoEscola == 1] # Intolerante para escolas públicas
y <- df$Privatista[df$TipoEscola == 2] # Intolerante para escolas privadas

# Executando o teste t de Student
t.test(x, y)

#
# criar o boxplot
boxplot(df$Intolerancia ~ df$Tipo_Escola,
        xlab = "Tipo de Escola",
        ylab = "Intolerante",
        main = "Distribuição de Intolerante por Tipo de Escola",
        col = c("lightblue", "lightgreen"))




# criar o boxplot
boxplot(df$Privatista ~ df$Tipo_Escola,
        xlab = "Tipo de Escola",
        ylab = "Privatista",
        main = "Distribuição de Privatista por Tipo de Escola",
        col = c("lightblue", "lightgreen"))





#
#extremista de esquerda e de direita

df$direita <- ifelse(df$q25EscalaDireitaEsquerda %in% c(55, 66, 88, 99, 100, 399), NA, df$q25EscalaDireitaEsquerda)
df$ExtremaDireita <- df$direita >8
df$ExtremaEsquerda <- df$direita <3.1

extremos <- subset(df, df$ExtremaDireita | df$ExtremaEsquerda)
extremos <- subset(extremos, select=c(ExtremaEsquerda, ExtremaDireita, Intolerancia,Privatista))


df$Ideologia <- ifelse(df$ExtremaEsquerda == TRUE, "Esquerda",
                       ifelse(df$ExtremaDireita == TRUE, "Direita", "Centro"))

df$Ideologia <- factor(df$Ideologia, levels = c("Esquerda", "Centro", "Direita"))

table(df$Ideologia)
extremos$Extremismo <- ifelse(extremos$ExtremaEsquerda == TRUE, "Esquerda",
                        ifelse(extremos$ExtremaDireita == TRUE, "Direita", "Nenhum"))
table(extremos$Extremismo)


#ideologia
boxplot(df$Intolerancia ~ df$Ideologia,
        xlab = "",
        ylab = "Intolerante",
        main = "Distribuição de Intolerante por Ideologia",
        col = c("red", "lightgreen","lightblue"))



# criar o boxplot
boxplot(df$Privatista ~ df$Ideologia,
        xlab = "",
        ylab = "Privatista",
        main = "Distribuição de Privatista por Ideologia",
        col = c("red", "lightgreen","lightblue"))

# teste de hipótese ideologia

# Execute a ANOVA
modeloInto <- aov(Intolerancia ~ Ideologia, data = df)

# Exiba os resultados
summary(modeloInto)
# Teste de Tukey
tukeyInto <- TukeyHSD(modeloInto, "Ideologia", conf.level = 0.9)
tukeyInto
plot(tukeyInto)


# Execute a ANOVA
modeloPriv <- aov(Privatista ~ Ideologia, data = df)

# Exiba os resultados
summary(modeloPriv)
# Teste de Tukey
tukeyPriv <- TukeyHSD(modeloPriv, "Ideologia", conf.level = 0.9)
tukeyPriv
plot(tukeyPriv)

#Meio de Informação
table(df$q41.2UtilizaParaInformarTelevisão)
df$Televisao <- df$q41.2UtilizaParaInformarTelevisão == 1
df$Televisao <- ifelse(df$Televisao, "MUITO", "POUCO/NADA")

table(df$q41.4UtilizaParaInformarFacebook)
df$Facebook <- df$q41.4UtilizaParaInformarFacebook == 1
df$Facebook <- ifelse(df$Facebook, "MUITO", "POUCO/NADA")

table(df$q41.6UtilizaParaInformarWhatsapp)
df$Whatsapp <- df$q41.6UtilizaParaInformarWhatsapp == 1
df$Whatsapp <- ifelse(df$Whatsapp, "MUITO", "POUCO/NADA")


table(df$q41.7UtilizaParaInformarYoutube)
df$Youtube <- df$q41.7UtilizaParaInformarYoutube == 1
df$Youtube <- ifelse(df$Youtube, "MUITO", "POUCO/NADA")


#
MeioCom <-c("Televisao","Televisao","Facebook", "Facebook",
            "Whatsapp","Whatsapp","Youtube","Youtube")
Consumo <-c("Muito","Pouco/Nada","Muito","Pouco/Nada","Muito","Pouco/Nada","Muito","Pouco/Nada")
Mean <- c(by(df$Intolerancia, df$Televisao, mean),
          by(df$Intolerancia, df$Facebook, mean),
          by(df$Intolerancia, df$Whatsapp, mean),
          by(df$Intolerancia, df$Youtube, mean))
Mean <- as.numeric(Mean)
graf.1 <- data.frame(MeioCom,Consumo, Mean)

# Definindo as cores para as barras
cores <- c("darkblue", "grey7")

# Criando o gráfico de barras
grafico_barras <- ggplot(data = graf.1, aes(x = MeioCom, y = Mean, fill = Consumo)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Mean, 2), y = Mean-0.01), position = position_dodge(width = 1), color = "white") +
  scale_fill_manual(values = cores, name = "Frequência em que se informa sobre política") +
  labs(title = "Intolerância versus Consumo de Mídias Sociais", x = "Tipo de Mídia", y = "Média de Intolerância") +
  theme_bw() + theme(legend.position = "bottom")

# Exibindo o gráfico
grafico_barras


Mean2 <- c(by(df$Privatista, df$Televisao, mean),
          by(df$Privatista, df$Facebook, mean),
          by(df$Privatista, df$Whatsapp, mean),
          by(df$Privatista, df$Youtube, mean))
Mean <- as.numeric(Mean2)
graf.2 <- data.frame(MeioCom,Consumo, Mean2)

# Criando o gráfico de barras
grafico_barras2 <- ggplot(data = graf.2, aes(x = MeioCom, y = Mean2, fill = Consumo)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Mean, 2), y = Mean-0.01), position = position_dodge(width = 1), color = "white") +
  scale_fill_manual(values = cores, name = "Frequência em que se informa sobre política") +
  labs(title = "Visão Pró Mercado versus Consumo de Mídias Sociais", x = "Tipo de Mídia", y = "Média de Visão Pró Mercado") +
  theme_bw() + theme(legend.position = "bottom")

# Exibindo o gráfico
grafico_barras2

#
# Transformar a variável dependente
# calcular o valor do percentil 75 da coluna "Intolerancia"
limite <- quantile(df$Intolerancia, probs = 0.75)

# criar uma nova coluna binária "Intolerantes" com valor TRUE se a coluna "Intolerancia" for maior ou igual ao limite, e FALSE caso contrário
df <- df %>%
  mutate(QuartilSuperiorIntolerantes = ntile(Intolerancia, 4) == 4)

table(df$QuartilSuperiorIntolerantes)#ok

df$FacebookMuito <- df$Facebook == "MUITO"
df$TelevisaoMuito <- df$Televisao == "MUITO"
df$WhatsappMuito <- df$Whatsapp == "MUITO"
df$YoutubeMuito <- df$Youtube == "MUITO"

# Rodar a regressão
df$Ideologia <- as.factor(df$Ideologia)
df$Ideologia <- relevel(df$Ideologia, "Centro")
modelo_QuartilSuperiorIntolerantes <- glm(QuartilSuperiorIntolerantes ~ Tipo_Escola+Ideologia+
                                FacebookMuito+WhatsappMuito+
                                TelevisaoMuito+YoutubeMuito, data=df,
                                family=binomial(link=logit))
tab_model(modelo_QuartilSuperiorIntolerantes)
df <- df %>%
  mutate(QuartilSuperiorPrivatistas = ntile(Privatista, 4) == 4)

table(df$QuartilSuperiorPrivatistas)#ok
modelo_QuartilSuperiorPrivatistas <- glm(QuartilSuperiorPrivatistas ~ Tipo_Escola+Ideologia+
                                           FacebookMuito+WhatsappMuito+
                                           TelevisaoMuito+YoutubeMuito, data=df,
                                         family=binomial(link=logit))
tab_model(modelo_QuartilSuperiorPrivatistas)

tab_model(modelo_QuartilSuperiorIntolerantes, modelo_QuartilSuperiorPrivatistas)


library(marginaleffects)
plot_cap(modelo_QuartilSuperiorIntolerantes, condition=c("WhatsappMuito","TelevisaoMuito"), conf_level = .9)
plot_cap(modelo_QuartilSuperiorIntolerantes, condition=c("WhatsappMuito","Ideologia"), conf_level = .9)
plot_cap(modelo_QuartilSuperiorPrivatistas, condition=c("Ideologia"))





# participação
table(df$q36aParticipouManifestações)
df$Manifest <- df$q36aParticipouManifestações == 1





t.test(df$Manifest, df$Intolerancia)
t.test(df$Manifest, df$Privatista)

boxplot(df$Privatista ~ df$Manifest,
        xlab = "",
        ylab = "Privatista",
        main = "Distribuição de Privatista por Participação em Manifestações",
        col = c("red", "lightgreen","lightblue"))
boxplot(df$Intolerancia ~ df$Manifest,
        xlab = "",
        ylab = "Intolerância",
        main = "Distribuição de Intolerância por Participação em Manifestações",
        col = c("red", "lightgreen","lightblue"))



#

by(df$Intolerancia, df$Manifest, mean)
by(df$Privatista, df$Manifest, mean)
Manif <-c("Não", "Sim","Não", "Sim")
Divis <-c("Intolerancia","Intolerancia","Privatista","Privatista")
Mean3 <- c(by(df$Intolerancia, df$Manifest, mean),
           by(df$Privatista, df$Manifest, mean))
Mean3 <- as.numeric(Mean3)
graf.3 <- data.frame(Manif,Divis, Mean3)

# Definindo as cores para as barras
cores <- c("lightgreen", "orange")

# Criando o gráfico de barras
grafico_barras3 <- ggplot(data = graf.3, aes(x = Manif, y = Mean3, fill = Manif)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Mean3, 2), y = Mean3-0.01), position = position_dodge(width = 1), color = "grey7") +
  scale_fill_manual(values = cores, name = "Participação em Manifestações?") +
  labs(title = "Média de Posicionamento em Issues e Participação em Manifestações", x = "Participação em Manifestações", y = "Médias") +
  theme_bw() + theme(legend.position = "bottom") + coord_flip() +
  facet_wrap(~ Divis, ncol = 1)

grafico_barras3


#
modelo_QuartilSuperiorPrivatistas2 <- glm(QuartilSuperiorPrivatistas ~ Tipo_Escola+Ideologia+
                                           FacebookMuito+WhatsappMuito+
                                           TelevisaoMuito+YoutubeMuito+Manifest,
                                          data=df,
                                         family=binomial(link=logit))
modelo_QuartilSuperiorIntolerantes2 <- glm(QuartilSuperiorIntolerantes ~ Tipo_Escola+Ideologia+
                                            FacebookMuito+WhatsappMuito+
                                            TelevisaoMuito+YoutubeMuito+Manifest,
                                           data=df,
                                          family=binomial(link=logit))



tab_model(modelo_QuartilSuperiorIntolerantes2, modelo_QuartilSuperiorPrivatistas2)

# modelo de equacões estruturais esboço
modelo_sem<-'
#equation where (endo) is predicted by (exogeneou)
q24.1_recod_numeric~q24.4_recod_numeric
q24.1_recod_numeric~q24.6_recod_numeric
q24.4_recod_numeric~q24.6_recod_numeric
Q32.2_recod_numeric~Q32.3_recod_numeric
Q32.2_recod_numeric~Q32.4_recod_numeric
Q32.2_recod_numeric~Q32.5_recod_numeric
Q32.3_recod_numeric~Q32.4_recod_numeric
Q32.3_recod_numeric~Q32.5_recod_numeric
Q32.4_recod_numeric~Q32.5_recod_numeric
#equation onde crio as divisoes
Intolerância =~ q24.1_recod_numeric + q24.4_recod_numeric + q24.6_recod_numeric
Visão_Privatista =~ Q32.2_recod_numeric + Q32.3_recod_numeric + Q32.4_recod_numeric + Q32.5_recod_numeric
#estimanting outro nível
Intolerância~FacebookMuito
Intolerância~WhatsappMuito
Intolerância~TelevisaoMuito
Intolerância~YoutubeMuito
Intolerância~EscolaPrivada
Intolerância~ExtremaDireita
Intolerância~ExtremaEsquerda
Visão_Privatista~FacebookMuito
Visão_Privatista~WhatsappMuito
Visão_Privatista~TelevisaoMuito
Visão_Privatista~YoutubeMuito
Visão_Privatista~EscolaPrivada
Visão_Privatista~ExtremaDireita
Visão_Privatista~ExtremaEsquerda'


# Ideia Dolezal 2022
#https://doi.org/10.1093/oxfordhb/9780198861126.013.28

#Participação (tipos) ~ Genero+TipoEscola+classeSubjetiva+ religiosidade +escolaridade dos pais+ VisaoEconomica+VisãoTolerância [logístico para tipo de participação)

#Participação Somatório~Genero+TipoEscola+classeSubjetiva religiosidade +escolaridade dos pais+ VisaoEconomica+VisãoTolerância (linear)

# primeiro as indep

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
df$religiosidade_numeric[is.na(df$religiosidade_numeric)] <- 2.340828
# agora falta inverter para valores maiores serem religiosidade
df$religiosidade <- memisc::recode(as.numeric(df$religiosidade_numeric),
                                          1 <- c(3),
                                          2 <- c(2.340828),
                                   2.340828<-c(2),
                                   3 <-c(1))
table(df$religiosidade)
summary(df$religiosidade)#ok
boxplot(df$religiosidade)

#escolaridade dos pais:
table(df$q59.1EscolaridadeMãe)
table(df$q59.2EscolaridadePai)
df$escolMae_num <- ifelse(df$q59.1EscolaridadeMãe > 7, NA, df$q59.1EscolaridadeMãe)
table(df$escolMae_num)
df$escolPai_num <- ifelse(df$q59.2EscolaridadePai > 7, NA, df$q59.2EscolaridadePai)
df$escolaridadePais <- df$escolMae_num + df$escolPai_num
table(df$escolaridadePais)
df$escolaridadePais <- scales::rescale(df$escolaridadePais, to = c(0, 1))
mean(df$escolaridadePais, na.rm = TRUE)
df$escolaridadePais[is.na(df$escolaridadePais)] <-  0.6487417
table(df$escolaridadePais)#ok!
sum(is.na(df$escolaridadePais))#OK!


summary(df$Intolerancia)#OK
summary(df$Privatista)#Ok


# Genero
table(df$q2Sexo)
sum(is.na(df$q2Sexo))
df$q2Sexo[is.na(df$q2Sexo)] <- 66
df$Genero <- factor(ifelse(df$q2Sexo == 1, "MASC", 
                           ifelse(df$q2Sexo == 2, "FEM",
                                  ifelse(df$q2Sexo == 66, "NR", NA))), 
                    levels = c("MASC", "FEM", "NR"))

table(df$Genero)
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

#6 index
df$IndexParticipacao <- df$OnLine + df$Protest + df$Votaria + df$Partisan
hist(df$IndexParticipacao,breaks=50)#Ok


# Modelos Particp 1,2,3,5,6index

modelo_Particip1 <- glm(Votaria ~ Genero+Tipo_Escola+Classe+
                          religiosidade+escolaridadePais+Intolerancia+Privatista,
                                           data=df,
                                           family=binomial(link=logit))

modelo_Particip2 <- glm(Partisan ~ Genero+Tipo_Escola+Classe+
                          religiosidade+escolaridadePais+Intolerancia+Privatista,
                        data=df,
                        family=binomial(link=logit))
modelo_Particip3 <- glm(Protest ~ Genero+Tipo_Escola+Classe+
                          religiosidade+escolaridadePais+Intolerancia+Privatista,
                        data=df,
                        family=binomial(link=logit))
modelo_Particip5 <- glm(OnLine ~ Genero+Tipo_Escola+Classe+
                          religiosidade+escolaridadePais+Intolerancia+Privatista,
                        data=df,
                        family=binomial(link=logit))
modelo_Particip6index <- lm(IndexParticipacao ~ Genero+Tipo_Escola+Classe+
                          religiosidade+escolaridadePais+Intolerancia+Privatista,
                        data=df)

tab_model(modelo_Particip1, modelo_Particip2,modelo_Particip3,modelo_Particip5)
tab_model(modelo_Particip6index)


plot_cap(modelo_Particip6index, condition=c("Genero"))
a<-plot_cap(modelo_Particip2, condition=c("Classe"),conf_level = .9)
b<-plot_cap(modelo_Particip5, condition=c("Classe"),conf_level = .9)
library(gridExtra)
grid.arrange(a,b,ncol=2)

plot_cap(modelo_Particip6index, condition = c("religiosidade"), conf_level = .9)

plot_cap(modelo_Particip2, condition = c("escolaridadePais"), conf_level = .9)
plot_cap(modelo_Particip2, condition = c("Intolerancia"), conf_level = .9)


#diag

# Obter os resíduos do modelo
residuos <- resid(modelo_Particip6index)

# Plotar o gráfico Normal Q-Q
qqnorm(residuos)
qqline(residuos)
library(olsrr)
ols_vif_tol(modelo_Particip6index)
ols_eigen_cindex(modelo_Particip6index)

library(car)
diagm1 <- vif(modelo_Particip1, partial = TRUE)
diagm1
diagm2 <- vif(modelo_Particip2, partial = TRUE)
diagm2
diagm3 <- vif(modelo_Particip3, partial = TRUE)
diagm3
diagm5 <- vif(modelo_Particip5, partial = TRUE)
diagm5

