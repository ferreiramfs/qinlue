acidentes <- read.csv2("data/acidentes.csv", fileEncoding = "UTF-8")
ideb <- read.csv2("data/ideb.csv", fileEncoding = "UTF-8")
nascimentos <- read.csv2("data/nascimentos.csv", fileEncoding = "UTF-8")
pib <- read.csv2("data/pib.csv", fileEncoding = "UTF-8")
populacao <- read.csv2("data/populacao.csv", fileEncoding = "UTF-8")

taxa_urbanizacao <- read.csv2("data/taxa_urbanizacao.csv", fileEncoding = "UTF-8")
jovens <- read.csv2("data/jovens.csv", fileEncoding = "UTF-8")
cids_alcool <- read.csv2("data/cids_alcool.csv", fileEncoding = "UTF-8")
equipes_saude <- read.csv2("data/equipes_saude.csv", fileEncoding = "UTF-8")

#Arrumando as bases
acidentes$cod_mun <- sub(" .*", "", acidentes$Município)
acidentes$nome_mun <- sub("^[^ ]+ ", "", acidentes$Município)
names(acidentes)[names(acidentes) == "Óbitos_p.Ocorrênc"] <- "acidentes"
acidentes$Município <- NULL

cids_alcool$cod_mun <- sub(" .*", "", cids_alcool$municipio)
cids_alcool$municipio <- NULL

nascimentos$cod_mun <- sub(" .*", "", nascimentos$Município)
nascimentos$Município <- NULL

equipes_saude$cod_mun <- sub(" .*", "", equipes_saude$Município)
equipes_saude$Município <- NULL

jovens$cod_mun <- sub(" .*", "", jovens$municipio)
jovens$municipio <- NULL

populacao$cod_mun <- sub(" .*", "", populacao$Município)
populacao$Município <- NULL

pib$cod_mun <- substr(pib$Código.do.Município, 1, nchar(pib$Código.do.Município) - 1)
pib$Nome.do.Município <- NULL
pib$Código.do.Município <- NULL

ideb$cod_mun <- substr(ideb$codigo, 1, nchar(ideb$codigo) - 1)
ideb$municipio <- NULL
ideb$codigo <- NULL

taxa_urbanizacao$cod_mun <- substr(taxa_urbanizacao$codigo, 1, nchar(taxa_urbanizacao$codigo) - 1)
taxa_urbanizacao$municipio <- NULL
taxa_urbanizacao$codigo <- NULL

raw_data <- merge(acidentes, equipes_saude, by = "cod_mun", all.x = TRUE)
raw_data <- raw_data[, c(1, 3, 2, 4)]
raw_data <- merge(raw_data, nascimentos, by = "cod_mun", all.x = TRUE)
raw_data <- merge(raw_data, populacao, by = "cod_mun", all.x = TRUE)
raw_data <- merge(raw_data, pib, by = "cod_mun", all.x = TRUE)
raw_data <- merge(raw_data, ideb, by = "cod_mun", all.x = TRUE)
raw_data <- merge(raw_data, jovens, by = "cod_mun", all.x = TRUE)
raw_data <- merge(raw_data, taxa_urbanizacao, by = "cod_mun", all.x = TRUE)
raw_data <- merge(raw_data, cids_alcool, by = "cod_mun", all.x = TRUE)

raw_data[] <- lapply(raw_data, function(x) replace(x, x == "-", 0))
raw_data <- data.frame(lapply(raw_data, type.convert, as.is = TRUE))

#IDEB para númerico
raw_data$ideb <- as.numeric(gsub(",", ".", raw_data$ideb))

#Transformando as taxas
raw_data$taxa_nascimentos <- round((raw_data$Nascim_p.ocorrênc / raw_data$População_estimada) * 1000, 2)
raw_data$taxa_jovens <- round((raw_data$pop_jovens / raw_data$População_estimada) * 1000, 2)
raw_data$taxa_internacoes_cidalcool <- round((raw_data$internacoes / raw_data$População_estimada) * 1000, 2)
raw_data$taxa_equipes_saude <- round((raw_data$Quantidade / raw_data$População_estimada) * 1000, 2)

cols_remover <- c("Valor.agropecuária", "Valor.Indústria", "Valor.Serviços", 'Valor.Administrativo', 'Total', 'Impostos',
                  'PIB', 'Óbitos_p.Ocorrênc', 'Nascim_p.ocorrênc', 'pop_jovens', 'internacoes', 'Quantidade')

raw_data <- raw_data[, !(names(raw_data) %in% cols_remover)]

names(raw_data)[names(raw_data) == "População_estimada"] <- "populacao"
names(raw_data)[names(raw_data) == "PIB.per.capita"] <- "pib_percap"

write.csv2(raw_data, 'data/dados.csv', row.names = FALSE)
