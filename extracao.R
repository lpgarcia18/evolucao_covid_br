#Setando ambiente --------------------------------------------------------
options(scipen=999)
gc()
set.seed(1)

# Pacotes -----------------------------------------------------------------
library(tidyverse)
# install.packages("remotes")
# remotes::install_github("georgevbsantiago/qsacnpj") - https://github.com/georgevbsantiago/qsacnpj
library(qsacnpj)
library(data.table)
library(basedosdados)
#devtools::install_github("tidyverse/dbplyr")
library(dbplyr)
library(keyring)

# Protegendo o Billing ----------------------------------------------------
key_set("project_billing", "basedosdados")

# Importando bases ------------------------------------------------------
#Definindo caminho relativo
path_bases <- paste0(getwd(),"/bases/")

# Setando conta
basedosdados::set_billing_id(key_get("project_billing", "basedosdados"))

# Carregar os dados do código de 7 dígitos do município
base <- tibble(
  query = 
  c("SELECT  
        ano,
        id_municipio,
        grupo_idade,
        SUM(populacao)
     FROM 
        `basedosdados.br_ms_populacao.municipio` 
    WHERE
        ano BETWEEN 2010 AND 2019
    GROUP BY
        ano,
        id_municipio,
        grupo_idade",
    "WITH obitos AS
    (SELECT 
        ano,
        id_municipio,
        numero_obitos,
        CASE 
          WHEN idade <= 4 THEN '0-4 anos' 
          WHEN idade > 4 AND idade <= 9 THEN '5-9 anos' 
          WHEN idade > 9 AND idade <= 14 THEN '10-14 anos' 
          WHEN idade > 14 AND idade <= 19 THEN '15-19 anos' 
          WHEN idade > 19 AND idade <= 24 THEN '20-24 anos' 
          WHEN idade > 24 AND idade <= 29 THEN '25-29 anos' 
          WHEN idade > 29 AND idade <= 34 THEN '30-34 anos' 
          WHEN idade > 34 AND idade <= 39 THEN '35-39 anos' 
          WHEN idade > 39 AND idade <= 44 THEN '40-44 anos' 
          WHEN idade > 44 AND idade <= 49 THEN '45-49 anos' 
          WHEN idade > 49 AND idade <= 54 THEN '50-54 anos' 
          WHEN idade > 54 AND idade <= 59 THEN '55-59 anos' 
          WHEN idade > 59 AND idade <= 64 THEN '60-64 anos' 
          WHEN idade > 64 AND idade <= 69 THEN '65-69 anos' 
          WHEN idade > 69 AND idade <= 74 THEN '70-74 anos' 
          WHEN idade > 74 AND idade <= 79 THEN '75-79 anos' 
          ELSE '80-mais' END AS grupo_idade
    FROM 
        `basedosdados.br_ms_sim.municipio_causa_idade` 
    WHERE 
        causa_basica LIKE 'C18%' AND
        ano BETWEEN '2010' AND '2019')
    SELECT
       ano,
       id_municipio,
       grupo_idade,
       SUM(numero_obitos)
    FROM
      obitos
    GROUP BY
      ano,
      id_municipio,
      grupo_idade")) %>% #Ano está como string
  mutate(resultados = map(query, read_sql))


# Transformando dados -----------------------------------------------------
base_ca <- merge(base$resultados[[1]], base$resultados[[2]], by = c("ano", "id_municipio", "grupo_idade"), all = T)
names(base_ca) <- c("ano", "id_municipio", "grupo_idade", "populacao", "obito")
base_ca$ano <- as.factor(base_ca$ano) 
base_ca$populacao <- as.numeric(base_ca$populacao) 
base_ca$obito <- as.numeric(base_ca$obito) 
base_ca$obito <- ifelse(is.na(base_ca$obito), 0, base_ca$obito)

write.csv(base_ca, paste0(path_bases, "base_ca.csv"), row.names = F)
# Calculando a taxa de mortalidade ----------------------------------------
# Taxa de mortalidade bruta
tx_bruta <- base_ca %>%
  group_by(ano, id_municipio) %>%
  dplyr::summarise(obito = sum(obito, na.rm = T),
                   populacao = sum(populacao, na.rm = T))
tx_bruta$tx_obito <- tx_bruta$obito/tx_bruta$populacao * 100000

