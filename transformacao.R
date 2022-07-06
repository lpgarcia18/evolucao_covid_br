#Setando ambiente --------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)

# Pacotes -----------------------------------------------------------------
library(tidyverse)
library(readr)


# Carregando a base -------------------------------------------------------
base_ca <- read_csv("bases/base_ca.csv")


# Compilando dados estaduais ----------------------------------------------
base_ca <- base_ca %>%
  group_by(ano, grupo_idade, id_uf, sigla_uf) %>%
  dplyr::summarise(populacao = sum(populacao, na.rm = T),
                   obito = sum(obito, na.rm = T))


# Calculando a taxa de mortalidade ----------------------------------------
#Calculando a população padrão
pop_padrao <- base_ca %>%
  group_by(ano, grupo_idade) %>%
  dplyr::summarise(pop_padrao = sum(populacao, na.rm = T))
base_ca <- merge(base_ca, pop_padrao, by = c("ano", "grupo_idade"), all = T)

#Calculando tx bruta reponderada
base_ca$tx_bruta <- base_ca$obito/base_ca$populacao
base_ca$tx_bruta_reponderada <- base_ca$tx_bruta * base_ca$pop_padrao
base_ca <- base_ca %>% 
  group_by(ano, id_uf, sigla_uf) %>%
  dplyr::summarise(tx_bruta_reponderada = sum(tx_bruta_reponderada, na.rm = T),
                   obito = sum(obito, na.rm = T),
                   populacao = sum(populacao, na.rm = T))

#Calculando pop total do Brasil por ano
pop_tot_ano <- base_ca %>%
  group_by(ano) %>%
  dplyr::summarise(pop_tot_ano = sum(populacao, na.rm = T))
base_ca <- merge(base_ca, pop_tot_ano, by = "ano", all = T)

#Calculando a tx padronizada para estado/ano
base_ca$tx_padronizada <- base_ca$tx_bruta_reponderada/base_ca$pop_tot_ano *100000

#Calculando a taxa brasileira para ano
base_ca_br <- base_ca %>%
  group_by(ano) %>%
  dplyr::summarise(obito = sum(obito, na.rm = T),
                   populacao = sum(populacao, na.rm = T))
base_ca_br$tx_bruta <- base_ca_br$obito/base_ca_br$populacao *100000
