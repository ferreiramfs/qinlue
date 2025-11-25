raw_data <- read.csv2('data/dados.csv')

set.seed(123)

ajustenb <- gamlss(acidentes ~ populacao + pib_percap + ideb + taxa_nascimentos + taxa_equipes_saude,
                   family = NBI, data = raw_data)

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

#Diferença quase nula ao modelar com sigma usando sqrt() como função de ligação, optamos por manter somente log() para mi

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