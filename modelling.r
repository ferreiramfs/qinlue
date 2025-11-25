packages_list <- c("ggplot2", "patchwork", "GGally", "tidyr", "car", "glmnet"
                   , "ggResidpanel", "hnp", "DHARMa", "gamlss", "gamlss.dist", "dplyr")

lapply(packages_list, library, character.only = TRUE)

raw_data <- read.csv2('data/dados.csv')

set.seed(123)

#Ajuste inicial Modelo Poisson
ajuste_pos <- gamlss(acidentes ~ . - cod_mun - nome_mun, family = PO, data = raw_data)
summary(ajuste_pos)

par(mfrow = c(2,2))
plot(ajuste_pos)

par(mfrow = c(1,1))
wp(ajuste_pos, xvar = NULL)

#Transformando as variáveis
raw_data$populacao <- log(raw_data$populacao)
raw_data$pib_percap <- log(raw_data$pib_percap)
raw_data$taxa_equipes_saude <- log1p(raw_data$taxa_equipes_saude)

ajuste_transf <- gamlss(acidentes ~ . - cod_mun - nome_mun, family = PO, data = raw_data)
summary(ajuste_transf)

par(mfrow = c(2,2))
plot(ajuste_transf)

par(mfrow = c(1,1))
wp(ajuste_transf, xvar = NULL)

AIC(ajuste_pos, ajuste_transf)

#Fazemos já uma seleção de variáveis utilizando LASSO no modelo base de Poisson
X <- model.matrix(acidentes ~ . - cod_mun - nome_mun, data = raw_data)[,-1]
y <- raw_data$acidentes

cvfit <- cv.glmnet(X, y, family = "poisson", alpha = 0.5)
coef(cvfit)

#Prosseguimos com modelos usando as variáveis selecionadas pelo LASSO
ajuste_lasso <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude
                       , family = PO, data = raw_data)
summary(ajuste_lasso)

par(mfrow = c(2,2))
plot(ajuste_lasso)

par(mfrow = c(1,1))
wp(ajuste_lasso, xvar = NULL)

#Parece ter indícios claros de superdispersão
#Tivemos um alternation limit reached, então aumentos o maxit para ter mais iterações
ajustenb <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude,
                   family = NBI, data = raw_data)
summary(ajustenb)

par(mfrow = c(1,1))
wp(ajustenb, xvar = NULL)

par(mfrow = c(2,2))
plot(ajustenb)

#Modelos para zeros inflacionados/alterados
ajusteZIP <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude,
                    sigma.formula =~ populacao,
                    family = ZIP, data = raw_data)
summary(ajusteZIP)
plot(ajusteZIP)

par(mfrow = c(1,1))
wp(ajusteZIP, xvar = NULL)

ajusteZAP <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude,
                    sigma.formula =~ populacao,
                    family = ZAP, data = raw_data)
summary(ajusteZAP)
plot(ajusteZAP)

par(mfrow = c(1,1))
wp(ajusteZAP, xvar = NULL)

ajusteZINBI <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude,
                      sigma.formula =~ populacao,
                      nu.formula =~ populacao,
                      family = ZINBI, data = raw_data, method = RS(3000))
summary(ajusteZINBI)
plot(ajusteZINBI)

par(mfrow = c(1,1))
wp(ajusteZINBI, xvar = NULL, ylim.all = TRUE)

ajusteZANBI <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude,
                      sigma.formula =~ populacao,
                      nu.formula =~ populacao,
                      family = ZANBI, data = raw_data, method = RS(300))

summary(ajusteZANBI)
plot(ajusteZANBI)

par(mfrow = c(1,1))
wp(ajusteZANBI, xvar = NULL, ylim.all = TRUE)

AIC(ajuste_lasso, ajustenb, ajusteZIP, ajusteZAP, ajusteZINBI, ajusteZANBI)
BIC(ajuste_lasso, ajustenb, ajusteZIP, ajusteZAP, ajusteZINBI, ajusteZANBI)

#Modelo NB tem o melhor desempenho, tanto graficamente como pelas métricas AIC e BIC
#Por isso, vamos prosseguir com ele para as próximas análises