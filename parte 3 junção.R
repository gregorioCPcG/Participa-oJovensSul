#OBS rodar POA e CTBA antes
tab_model(modelo_Particip1, modelo_Particip2,modelo_Particip3,
          modelo_Particip5,modelo_Particip6index,
          show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")#CTBA


hist(Poa$Intolerancia)
hist(Poa$Privatista)


hist(df$Intolerancia)
hist(df$Privatista)
Ctba <- subset(df, select=c(Votaria,Partisan,Protest,OnLine,IndexParticipacao,Genero,Tipo_Escola,Classe,
                 religiosidade,escolaridadePais,Intolerancia,Privatista))
Ctba$Contexto <- "Curitiba 2022"

Poa<- subset(Poa, select=c(Votaria,Partisan,Protest,OnLine,IndexParticipacao,Genero,Tipo_Escola,Classe,
                            religiosidade,escolaridadePais,Intolerancia,Privatista))
Poa$Contexto <- "Porto Alegre 2019"

df <- full_join(Ctba,Poa)
summary(df)

table(df$Contexto)
df$Contexto <- as.factor(df$Contexto)
table(df$Tipo_Escola)

# Modelos Particp 1,2,3,5,6index
modelo_Particip1 <- glm(Votaria ~ Genero+Tipo_Escola+Classe+
                          religiosidade+escolaridadePais+Intolerancia+Privatista+Contexto,
                        data=df,
                        family=binomial(link=logit))

modelo_Particip2 <- glm(Partisan ~ Genero+Tipo_Escola+Classe+
                          religiosidade+escolaridadePais+Intolerancia+Privatista+Contexto,
                        data=df,
                        family=binomial(link=logit))
modelo_Particip3 <- glm(Protest ~ Genero+Tipo_Escola+Classe+
                          religiosidade+escolaridadePais+Intolerancia+Privatista+Contexto,
                        data=df,
                        family=binomial(link=logit))
modelo_Particip5 <- glm(OnLine ~ Genero+Tipo_Escola+Classe+
                          religiosidade+escolaridadePais+Intolerancia+Privatista+Contexto,
                        data=df,
                        family=binomial(link=logit))
modelo_Particip6index <- lm(IndexParticipacao ~ Genero+Tipo_Escola+Classe+
                              religiosidade+escolaridadePais+Intolerancia+Privatista+Contexto,
                            data=df)

tab_model(modelo_Particip1, modelo_Particip2,modelo_Particip3,modelo_Particip5)
tab_model(modelo_Particip6index)

tab_model(modelo_Particip1, modelo_Particip2,modelo_Particip3,
          modelo_Particip5,modelo_Particip6index,
          show.ci = F, auto.label = T, show.se = T, collapse.se = T, 
          wrap.labels = 60, p.style = "stars")#fusionada

a<- plot_cap(modelo_Particip6index, condition=c("Genero"), conf_level = .9)
a + theme_bw()
b <- plot_cap(modelo_Particip6index, condition=c("Privatista", "Genero"), conf_level = .9)
b + theme_bw()
c <- plot_cap(modelo_Particip6index, condition=c("Intolerancia", "Genero"), conf_level = .9)
c + theme_bw()
c <- plot_cap(modelo_Particip6index, condition=c("Intolerancia", "Genero","Privatista"), conf_level = .9)
c + theme_bw()
