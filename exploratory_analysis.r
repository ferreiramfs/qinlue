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
          , 'taxa_internacoes_cidalcool', 'taxa_equipes_saude', 'taxa_jovens')

summary(raw_data[c('acidentes', vars)])

plots <- lapply(vars, function(v) {
  ggplot(raw_data, aes_string(x = v, y = 'acidentes')) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    theme_bw() +
    labs(title = v)
})

wrap_plots(plots, ncol = 4)
