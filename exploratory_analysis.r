packages_list <- c("ggplot2", "patchwork", "GGally", "tidyr", "car", "glmnet"
                   , "ggResidpanel", "hnp", "DHARMa", "gamlss", "gamlss.dist", "dplyr")

lapply(packages_list, library, character.only = TRUE)

raw_data <- read.csv2('data/dados.csv')

set.seed(123)
#Análise exploratória das variáveis
summary(raw_data[-cod_mun])

#As transformação a seguir foram aplicada inicialmente SOMENTE para melhor 
#visualizar os dados, porém os primeiros modelos rodar sem as transformações
#e NÂO transformar a variável resposta para nenhum modelo

raw_data$populacao <- log(raw_data$populacao)
raw_data$pib_percap <- log(raw_data$pib_percap)
raw_data$taxa_equipes_saude <- log1p(raw_data$taxa_equipes_saude)
raw_data$acidentes <- log1p(raw_data$acidentes)

hist(raw_data$acidentes, breaks = 10, main = "Distribuição de Acidentes")

vars <- c('populacao', 'pib_percap', 'taxa_nascimentos', 'taxa_urb', 'ideb'
          , 'taxa_internacoes_cidalcool', 'taxa_equipes_saude')

summary(raw_data[c('acidentes', vars)])

plots <- lapply(vars, function(v) {
  ggplot(raw_data, aes_string(x = v, y = 'acidentes')) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    theme_bw() +
    labs(title = v)
})

wrap_plots(plots, ncol = 4)

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

#Testando outra funções de ligação
ajustenb_sqrt <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude,
                   family = NBI(mu.link = "sqrt"), data = raw_data)

summary(ajustenb)
summary(ajustenb_sqrt)

ajustenb_sigsqrt <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude,
                           sigma.formula =~ populacao,
                        family = NBI(sigma.link = "sqrt"), data = raw_data)

ajustenb_sigsqrt_total <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude,
                           sigma.formula =~ populacao + taxa_nascimentos + taxa_equipes_saude,
                           family = NBI(sigma.link = "sqrt"), data = raw_data)

ajustenb_sigsqrt2_total <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude,
                                 sigma.formula =~ populacao + taxa_nascimentos + taxa_equipes_saude,
                                 family = NBI(mu.link = "sqrt", sigma.link = "sqrt"), data = raw_data)

ajustenb_sig_total <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude,
                                 sigma.formula =~ populacao + taxa_nascimentos + taxa_equipes_saude,
                                 family = NBI, data = raw_data)

summary(ajustenb_sig_total)
summary(ajustenb_sigsqrt_total)

AIC(ajustenb, ajustenb_sqrt, ajustenb_sigsqrt, ajustenb_sigsqrt_total, ajustenb_sig_total, ajustenb_sigsqrt2_total)
BIC(ajustenb, ajustenb_sqrt, ajustenb_sigsqrt, ajustenb_sigsqrt_total, ajustenb_sig_total, ajustenb_sigsqrt2_total)

par(mfrow = c(1,1))
wp(ajustenb_sigsqrt_total, xvar = NULL)

par(mfrow = c(2,2))
plot(ajustenb_sigsqrt_total)

#Seleção de variáveis método STEP
step(ajustenb)

ajustenb_step <- gamlss(acidentes ~ populacao + taxa_equipes_saude,
                   family = NBI, data = raw_data)

AIC(ajustenb, ajustenb_step)
BIC(ajustenb, ajustenb_step)

summary(ajustenb_step)

par(mfrow = c(1,1))
wp(ajustenb, xvar = NULL)

par(mfrow = c(2,2))
plot(ajustenb)

#Caso de exemplo interpretação
dados_exemplo <- raw_data[c(1, 89, 105),c('nome_mun','acidentes', 'populacao', 'taxa_equipes_saude')]
mat_cov <- vcov(ajustenb_step, what="mu")

pred <- predictAll(ajustenb_step, newdata = dados_exemplo, type = "link")

LP     <- pred$mu.fit
SE_LP  <- pred$mu.se

mu_hat     <- exp(LP)
IC_low_mu  <- exp(LP - 1.96 * SE_LP)
IC_high_mu <- exp(LP + 1.96 * SE_LP)

data.frame(
  dados_exemplo,
  mu_hat,
  IC_low_mu,
  IC_high_mu
)

#Pontos influentes
rq <- resid(ajustenb_step, what = "z-scores")
H <- hatvalues(ajustenb_step)
infl_points <- which(H > 3 * mean(H))

summary(raw_data[, c('acidentes', 'populacao', 'taxa_equipes_saude')])

raw_no_infl <- raw_data[-infl_points, ]
summary(ajustenb_step)

ajuste_no_infl <- gamlss(acidentes ~ populacao + taxa_equipes_saude,
                         family = NBI, data = raw_no_infl)

summary(ajuste_no_infl)
plot(ajuste_no_infl)
plot(ajustenb_step)

par(mfrow = c(1,1))
wp(ajustenb_step, xvar = NULL)
wp(ajuste_no_infl, xvar = NULL)

AIC(ajustenb_step, ajuste_no_infl)
BIC(ajustenb_step, ajuste_no_infl)
