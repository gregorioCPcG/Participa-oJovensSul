# replicar POA EM CTBA
#  rodar POA (deixaar salvo) ANTES 

df -> Poa
Ctba -> curitiba_completo
library(labelled)
Ctba <- remove_labels(Ctba)
table(Ctba$Q18.9OpíniãoDemocraciaMelhorFormaGoverno)#usar - valores positivos antidemoc
table(Ctba$Q18.10OpíniãoAutoritárioMelhorDemocráticos)# valores negativos antidemoc inverter
Ctba$Q18.9r <- Ctba$Q18.9OpíniãoDemocraciaMelhorFormaGoverno

Ctba$Q18.10.r <- memisc::recode(as.numeric(Ctba$Q18.10OpíniãoAutoritárioMelhorDemocráticos),
                                           1 <- c(3),
                                           2 <- c(2),
                                           3<-c(1))
table(Ctba$Q18.10.r)
cor.test(Ctba$Q18.9r, Ctba$Q18.10.r)

Ctba$Q18.9r <- scales::rescale(Ctba$Q18.9r, to = c(0, 1))
Ctba$Q18.10.r <- scales::rescale(Ctba$Q18.10.r, to = c(0, 1))


table(Ctba$Q23.1IncomodaOutrasReligiões)#3 é tolerante inverter
table(Ctba$Q23.4IncomodaHomossexuais)#3 é tolerante inverter
table(Ctba$Q23.6IncomodaOpiniõesPolíticas)#3 é tolerante inverter

Ctba$Q23.1R <- memisc::recode(as.numeric(Ctba$Q23.1IncomodaOutrasReligiões),#inverter para valores negativos ser intolerante
                                          1 <- c(3),
                                          2 <- c(2),
                                          3<-c(1))
Ctba$Q23.4R <- memisc::recode(as.numeric(Ctba$Q23.4IncomodaHomossexuais),#inverter para valores negativos ser intolerante
                                          1 <- c(3),
                                          2 <- c(2),
                                          3<-c(1))
Ctba$Q23.6R <- memisc::recode(as.numeric(Ctba$Q23.6IncomodaOpiniõesPolíticas),#inverter para valores negativos ser intolerante
                                          1 <- c(3),
                                          2 <- c(2),
                                          3<-c(1))

Ctba$Q23.6R <- scales::rescale(Ctba$Q23.6R, to = c(0, 1))
Ctba$Q23.4R <- scales::rescale(Ctba$Q23.4R, to = c(0, 1))
Ctba$Q23.1R <- scales::rescale(Ctba$Q23.1R, to = c(0, 1))
table(Ctba$Q23.6R)
table(Ctba$Q23.6IncomodaOpiniõesPolíticas)


Ctba$Q29.1GovernoMercadoDonoEmpresas -> Ctba$Q29.1R
table(Ctba$Q29.1R)
Ctba$Q29.2GovernoMercadoBemEstarCidadãos -> Ctba$Q29.2R
Ctba$Q29.3GovernoMercadoAposentadorias -> Ctba$Q29.3R
Ctba$Q29.4GovernoMercadoSaúde-> Ctba$Q29.4R
Ctba$Q29.5GovernoMercadoEducação-> Ctba$Q29.5R
Ctba$Q29.1R <- scales::rescale(Ctba$Q29.1R, to = c(0, 1))
Ctba$Q29.2R <- scales::rescale(Ctba$Q29.2R, to = c(0, 1))
Ctba$Q29.3R <- scales::rescale(Ctba$Q29.3R, to = c(0, 1))
Ctba$Q29.4R <- scales::rescale(Ctba$Q29.4R, to = c(0, 1))
Ctba$Q29.5R <- scales::rescale(Ctba$Q29.5R, to = c(0, 1))
# CFA
Ctba -> Ctba_completo
Ctba <- Ctba[,286:295]
summary(Ctba)


imp <- mice(Ctba, seed=23109)#a seed sempre essa 23109
Ctba <- complete(imp, 1)#sempre a m como  destino da mputação , empre escolher a 1
rm(imp)
summary(Ctba)

scree(Ctba)
nfactors(Ctba)
fa2 <-psych::fa(Ctba,2)
fa2$loadings
fa3 <-psych::fa(Ctba,3)
fa3$loadings



modelo_cfa <- '
  Intolerância =~ Q23.1R + Q23.4R + Q23.6R
  Visão_Privatista =~ Q29.2R + Q29.3R + Q29.4R + Q29.5R'
ajuste_cfa <- cfa(modelo_cfa, data = Ctba,check.gradient = FALSE)

summary(ajuste_cfa, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)
semTools::fitmeasures(ajuste_cfa, c("tli", "cfi", "rmsea"))


lavaanPlot(model =ajuste_cfa, node_options = list(shape = "circle", fontname = 
                                                    "Garamond"), edge_options = list(color = "blue"), coefs = T,covs=
             F,stars = TRUE)





modelo_cfa_alternativo <- '
  Autoritarismo =~ Q18.9r+Q18.10.r
  Intolerância =~ Q23.1R + Q23.4R + Q23.6R
  Visão_Privatista =~ Q29.2R + Q29.3R + Q29.4R + Q29.5R'
ajuste_cfa_alternativo <- cfa(modelo_cfa_alternativo, data = Ctba,check.gradient = FALSE)
semTools::fitmeasures(ajuste_cfa, c("tli", "cfi", "rmsea"))
semTools::fitmeasures(ajuste_cfa_alternativo, c("tli", "cfi", "rmsea"))
summary(ajuste_cfa_alternativo, fit.measures = TRUE, standardized = TRUE, modindices = TRUE)


lavaanPlot(model =ajuste_cfa_alternativo, node_options = list(shape = "circle", fontname = 
                                                                "Garamond"), edge_options = list(color = "blue"), coefs = T,covs=
             F,stars = TRUE)


scores <- lavPredict(ajuste_cfa_alternativo)
Ctba_completo -> df
scores[,1] -> df$Autoritarismo 
scores[,2] -> df$Intolerancia 
scores[,3] -> df$Privatista 


df$log10_Autoritarismo <- log10(df$Autoritarismo)
df$log10_Privatista <- log10(df$Privatista)
df$log10_Intolerancia <- log10(df$Intolerancia)
df$Intolerancia <- scales::rescale(df$Intolerancia, to = c(0, 1))
df$Privatista <- scales::rescale(df$Privatista, to = c(0, 1))
df$Autoritarismo <- scales::rescale(df$Autoritarismo, to = c(0, 1))

hist(df$log10_Autoritarismo)
hist(df$log10_Intolerancia)
hist(df$log10_Privatista)

table(df$TipoEscola)
sum(is.na(df$TipoEscola))
df$TipoEscola <- ifelse(df$TipoEscola == "Federal", NA, df$TipoEscola)


table(df$Q24EscalaEsquerdaDireita)
sum(is.na(df$Q24EscalaEsquerdaDireita))
df$Q24EscalaEsquerdaDireita <- ifelse(is.na(df$Q24EscalaEsquerdaDireita), 66, df$Q24EscalaEsquerdaDireita)
df$Ideologia <- ifelse(df$Q24EscalaEsquerdaDireita %in% c(1,2,3), "Esquerda",
                                      ifelse(df$Q24EscalaEsquerdaDireita %in% c(4,5,6), "Centro",
                                             ifelse(df$Q24EscalaEsquerdaDireita %in% c(7,8,9,10), "Direita", "NS/NR")))

df$Ideologia <- factor(df$Q24EscalaEsquerdaDireita, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 66),
                                      labels = c("Esquerda", "Esquerda", "Esquerda", "Centro", "Centro", "Centro",
                                                 "Direita", "Direita", "Direita", "Direita", "NS/NR"))
table(df$Ideologia)


print(curitiba_completo$Q36.2InformarTelevisão)
df$TelevisaoMuito <- df$Q36.2InformarTelevisão == 1
df$FacebookMuito <- df$Q36.4InformarFacebook == 1
df$WhatsappMuito <- df$Q36.6InformarWhatsapp == 1
df$YoutubeMuito <- df$Q36.7InformarYoutube == 1
df$TikTokMuito <- df$Q36.9InformarTikTok == 1

modeloCTBAAutoritarismo <- lm(log10_Autoritarismo ~ Ideologia+TipoEscola+
                                TelevisaoMuito+FacebookMuito+
                                WhatsappMuito+YoutubeMuito+
                                TikTokMuito,
                              data=df)
modeloCTBAPrivatista <- lm(log10_Privatista ~ Ideologia+TipoEscola+
                                TelevisaoMuito+FacebookMuito+
                                WhatsappMuito+YoutubeMuito+
                                TikTokMuito,
                              data=df)
modeloCTBAIntolerancia <- lm(log10_Intolerancia~ Ideologia+TipoEscola+
                                TelevisaoMuito+FacebookMuito+
                                WhatsappMuito+YoutubeMuito+
                                TikTokMuito,
                              data=df)
tab_model(modeloCTBAIntolerancia, modeloCTBAAutoritarismo, modeloCTBAPrivatista)

df <- df %>%
  mutate(TercilSuperiorIntolerantes = ntile(Intolerancia, 3) == 3)
df <- df %>%
  mutate(TercilSuperiorAutoritarios = ntile(Autoritarismo, 3) == 3)
df <- df %>%
  mutate(TercilSuperiorPrivatista = ntile(Privatista, 3) == 3)

modeloCTBAAutoritarismo33top <- glm(TercilSuperiorAutoritarios ~ Ideologia+TipoEscola+
                                TelevisaoMuito+FacebookMuito+
                                WhatsappMuito+YoutubeMuito+
                                TikTokMuito,data=df,
                                family=binomial(link=logit))
modeloCTBAIntolerancia33top <- glm(TercilSuperiorIntolerantes ~ Ideologia+TipoEscola+
                             TelevisaoMuito+FacebookMuito+
                             WhatsappMuito+YoutubeMuito+
                             TikTokMuito,data=df,
                             family=binomial(link=logit))
modeloCTBAPrivatista33top <- glm(TercilSuperiorPrivatista~ Ideologia+TipoEscola+
                               TelevisaoMuito+FacebookMuito+
                               WhatsappMuito+YoutubeMuito+
                               TikTokMuito,data=df,
                               family=binomial(link=logit))
tab_model(modeloCTBAIntolerancia33top,
          modeloCTBAAutoritarismo33top,
          modeloCTBAPrivatista33top)
plot_cap(modeloCTBAIntolerancia33top, condition=c("Ideologia"),conf_level = .9)
plot_cap(modeloCTBAPrivatista33top, condition=c("Ideologia"),conf_level = .9)
plot_cap(modeloCTBAAutoritarismo33top, condition=c("WhatsappMuito"),conf_level = .9)
plot_cap(modeloCTBAPrivatista33top, condition=c("FacebookMuito"),conf_level = .9)
library(Hmisc)
mcorr <- rcorr(as.matrix(df[,296:298]))
mcorr

by(df$Intolerancia, df$Q36.6InformarWhatsapp, mean)
df$Whats <- ifelse(df$Q36.6InformarWhatsapp == 1, "Muito",
                   ifelse(df$Q36.6InformarWhatsapp == 2, "Pouco",
                          ifelse(df$Q36.6InformarWhatsapp == 3, "Nada", NA)))

df$Whats <- factor(df$Whats, levels = c("Nada", "Pouco", "Muito"))

by(df$Intolerancia, df$Whats, mean)
modeloAOVINTOCTBA <- aov(Intolerancia ~ Whats, data = df)

# Exiba os resultados
summary(modeloAOVINTOCTBA)
# Teste de Tukey
tukey <- TukeyHSD(modeloAOVINTOCTBA, "Whats", conf.level = 0.9)
tukey
plot(tukey)


# participação

table(df$Q31.6ParticipaçãoManifestaçõesProtestos)
df$Manifest1 <- df$Q31.6ParticipaçãoManifestaçõesProtestos < 3
table(df$Manifest1)# 77 indivi

table(df$Q32ParticipouManifestações)
df$Manifest2_bra <- df$Q32ParticipouManifestações ==2
table(df$Manifest2_bra)# 26 indivi


t.test(df$Manifest1, df$Intolerancia)
t.test(df$Manifest1, df$Privatista)
t.test(df$Manifest1, df$Autoritarismo)
t.test(df$Manifest2_bra, df$Privatista)
t.test(df$Manifest2_bra, df$Intolerancia)
t.test(df$Manifest2_bra, df$Autoritarismo)



by(df$Intolerancia, df$Manifest1, mean)
by(df$Privatista, df$Manifest1, mean)
Manif2 <-c("Não", "Sim","Não", "Sim","Não","Sim")
Divis2 <-c("Intolerancia","Intolerancia","Privatista","Privatista",
           "Autoritarismo","Autoritarismo")
Mean4 <- c(by(df$Intolerancia, df$Manifest1, mean),
           by(df$Privatista, df$Manifest1, mean),
           by(df$Autoritarismo, df$Manifest1, mean))
Mean4 <- as.numeric(Mean4)
graf.4 <- data.frame(Manif2,Divis2, Mean4)

# Definindo as cores para as barras
cores <- c("lightgreen", "orange")

# Criando o gráfico de barras
grafico_barras4 <- ggplot(data = graf.4, aes(x = Manif2, y = Mean4, fill = Manif2)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Mean4, 2), y = Mean4-0.01), position = position_dodge(width = 1), color = "grey7") +
  scale_fill_manual(values = cores, name = "Participação em Manifestações? (Q31.6)") +
  labs(title = "Média de Posicionamento em Issues e Participação em Manifestações", x = "Participação em Manifestações", y = "Médias") +
  theme_bw() + theme(legend.position = "bottom") + coord_flip() +
  facet_wrap(~ Divis2, ncol = 1)

grafico_barras4



Manif22 <-c("Não", "Sim","Não", "Sim","Não","Sim")
Divis22 <-c("Intolerancia","Intolerancia","Privatista","Privatista",
           "Autoritarismo","Autoritarismo")
Mean42 <- c(by(df$Intolerancia, df$Manifest2_bra, mean),
           by(df$Privatista, df$Manifest2_bra, mean),
           by(df$Autoritarismo, df$Manifest2_bra, mean))
Mean42 <- as.numeric(Mean42)
graf.42 <- data.frame(Manif22,Divis22, Mean42)

# Criando o gráfico de barras
grafico_barras42 <- ggplot(data = graf.42, aes(x = Manif22, y = Mean42, fill = Manif22)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Mean42, 2), y = Mean42-0.01), position = position_dodge(width = 1), color = "grey7") +
  scale_fill_manual(values = cores, name = "Participação em Manifestações? (Q32)") +
  labs(title = "Média de Posicionamento em Issues e Participação em Manifestações", x = "Participação em Manifestações", y = "Médias") +
  theme_bw() + theme(legend.position = "bottom") + coord_flip() +
  facet_wrap(~ Divis22, ncol = 1)

grafico_barras42


df$Manifest2_bra22 <- as.factor(df$Manifest2_bra)
levels(df$Manifest2_bra22)


modeloAOVINTOCTBA22 <- aov(Intolerancia ~ Manifest2_bra22, data = df)

# Exiba os resultados
summary(modeloAOVINTOCTBA22)
# Teste de Tukey
tukey22 <- TukeyHSD(modeloAOVINTOCTBA22, "Manifest2_bra22", conf.level = 0.9)
tukey22
plot(tukey22)


library("PerformanceAnalytics")
my_data <- df[,296:298]
chart.Correlation(my_data, histogram=T, pch=19)


hist(df$Intolerancia,breaks=200)
hist(df$Autoritarismo,breaks=200)
hist(df$Privatista,breaks=200)



#

#como em POA
## Ideia Dolezal 2022
#https://doi.org/10.1093/oxfordhb/9780198861126.013.28

#Participação (tipos) ~ Genero+TipoEscola+classeSubjetiva+ religiosidade +escolaridade dos pais+ VisaoEconomica+VisãoTolerância [logístico para tipo de participação)

#Participação Somatório~Genero+TipoEscola+classeSubjetiva religiosidade +escolaridade dos pais+ VisaoEconomica+VisãoTolerância (linear)

# primeiro as indep


table(df$TipoEscola)
df$Tipo_Escola <- df$TipoEscola
sum(is.na(df$Tipo_Escola))#6 aceitável ok

##classe:
table(df$Q68ClasseSocial)#
# Criar nova variável Classe
df$Classe <- ifelse(df$Q68ClasseSocial == 1, "Baixo(pobre)",
                    ifelse(df$Q68ClasseSocial == 2, "Média Baixa",
                           ifelse(df$Q68ClasseSocial == 3, "Média",
                                  ifelse(df$Q68ClasseSocial %in% c(4,5), "Média Alta e Alta(rico)",NA))))
table(df$Classe)
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
table(df$Q31.3ParticipaçãoAtividadesReligiosas)#proxy de religiosidade
sum(is.na(df$Q31.3ParticipaçãoAtividadesReligiosas))
# 1 muito religioso
# 2 um pouoc
# 3 nada
df$religiosidade_numeric <- ifelse(df$Q31.3ParticipaçãoAtividadesReligiosas > 3, NA, df$Q31.3ParticipaçãoAtividadesReligiosas)
mean(df$religiosidade_numeric, na.rm = TRUE)
#imputar média para NA
table(df$religiosidade_numeric)
summary(df$religiosidade_numeric)
df$religiosidade_numeric[is.na(df$religiosidade_numeric)] <- 2.094488
# agora falta inverter para valores maiores serem religiosidade
df$religiosidade <- memisc::recode(as.numeric(df$religiosidade_numeric),
                                   1 <- c(3),
                                   2 <- c(2.094488),
                                   2.094488<-c(2),
                                   3 <-c(1))
table(df$religiosidade)
summary(df$religiosidade)#ok
boxplot(df$religiosidade)

#escolaridade dos pais:
table(df$Q58EscolaridadeMãe)
table(df$Q59EscolaridadePai)
df$escolMae_num <- df$Q58EscolaridadeMãe
table(df$escolMae_num)
df$escolPai_num <- df$Q59EscolaridadePai
df$escolaridadePais <- df$escolMae_num + df$escolPai_num
table(df$escolaridadePais)
df$escolaridadePais <- scales::rescale(df$escolaridadePais, to = c(0, 1))
mean(df$escolaridadePais, na.rm = TRUE)
df$escolaridadePais[is.na(df$escolaridadePais)] <-  0.6141104
table(df$escolaridadePais)#ok!
sum(is.na(df$escolaridadePais))#OK!

summary(df$Intolerancia)#OK
summary(df$Privatista)#Ok


# Genero
table(df$Sexo)
sum(is.na(df$Sexo))
df$q2Sexo[is.na(df$Sexo)] <- 66
df$Genero <- factor(ifelse(df$Sexo == 1, "MASC", 
                           ifelse(df$Sexo == 2, "FEM", NA)), 
                    levels = c("MASC", "FEM"))

table(df$Genero)#deu!

#

# as deps

# 1- voto

table(df$Q20VotariaSeNãoFosseObrigatório)#NA considerar
df$Votaria <- df$Q20VotariaSeNãoFosseObrigatório
sum(is.na(df$Votaria))#40 NAS faze oq 
table(df$Votaria)#inverter e lógica 1x0
df$Votaria <- memisc::recode(as.numeric(df$Votaria),
                             1 <- c(1),
                             0 <- c(2))
table(df$Votaria)#Ok 236


# 2- partisan
table(df$Q31.1ParticipaçãoPartidosPolíticos)#NA considerar
df$Partisan <- df$Q31.1ParticipaçãoPartidosPolíticos
sum(is.na(df$Partisan))#98 NAS faze oq 
table(df$Partisan)# q m (mesmo problema, poucos sim)
#poucos casos - binarizar 1 e 2 como 1 e 3 como zero
df$Partisan <- memisc::recode(as.numeric(df$Partisan),
                              1 <- c(1,2),
                              0 <- c(3))
table(df$Partisan)#Ok 13


#3 - protest (proxy)
table(df$Q31.6ParticipaçãoManifestaçõesProtestos)#NA considerar
df$Protest <- df$Q31.6ParticipaçãoManifestaçõesProtestos
table(df$Protest)
#binarizar 1 e 2 como 1 e 3 como zero
df$Protest <- memisc::recode(as.numeric(df$Protest),
                             1 <- c(1,2),
                             0 <- c(3))
table(df$Protest)#Ok 77
sum(is.na(df$Protest))#99 NAS faze oq


#4 nao tem

#5 online
table(df$Q31.10ParticipaçãoMobilizaçõesRedesSociais)#NA considerar
df$OnLine <- df$Q31.10ParticipaçãoMobilizaçõesRedesSociais
table(df$OnLine)
#poucos casos - binarizar 1 e 2 como 1 e 3 como zero
df$OnLine <- memisc::recode(as.numeric(df$OnLine),
                            1 <- c(1,2),
                            0 <- c(3))
table(df$OnLine)#Ok 321
sum(is.na(df$OnLine))#100 NAS faze oq

#6 index
df$IndexParticipacao <- df$OnLine + df$Protest + df$Votaria + df$Partisan
hist(df$IndexParticipacao,breaks=50)#Ok
sum(is.na(df$IndexParticipacao))#100 NAS faze oq

# Modelos Particp 1,2,3,5,6index
modelo_Particip1 <- glm(Votaria ~ Genero+Tipo_Escola+Classe+
                          religiosidade+escolaridadePais+Intolerancia+Privatista,
                        data=df,
                        family=binomial(link=logit))

modelo_Particip2 <- glm(Partisan ~ Genero+Tipo_Escola+
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
plot_cap(modelo_Particip6index, condition=c("Genero"), conf_level = .9)

# modelo adicional democracia

modelo_Particip6indexB <- lm(IndexParticipacao ~ Genero+Tipo_Escola+Classe+
                              religiosidade+escolaridadePais+Intolerancia+Privatista,
                            data=df)
tab_model(modelo_Particip6indexB)


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
