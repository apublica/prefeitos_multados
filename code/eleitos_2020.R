library(tidyverse)
library(basedosdados)

### BD

set_billing_id("xxx")

## Base de resultados
query <- bdplyr("br_tse_eleicoes.resultados_candidato") %>%
  filter(ano ==2020, cargo == "prefeito", resultado != "nao eleito")

prefs20_eleitos <- bd_collect(query)
## Base de candidatos

query2 <- bdplyr("br_tse_eleicoes.candidatos") %>%
  select(ano, sigla_uf, id_candidato_bd,
         nome, situacao, ocupacao, genero,
         sequencial, instrucao, raca, cpf, titulo_eleitoral) %>%
  filter(ano == 2020)

df_candidatos <- bd_collect(query2)
prefs20_eleitos$sequencal <- prefs20_eleitos$sequencial_candidato

prefs20_eleitos2 <- prefs20_eleitos %>%
  left_join(df_candidatos, by = c("sequencial")) %>%
  rename(sigla_uf = sigla_uf.x) 


write.csv2(prefs20_eleitos2, "prefs20_eleitos.csv")