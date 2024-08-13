library(tidyverse)
library(janitor)
library(lubridate)

df_multas <- read_csv("data/multas1308.csv") |> janitor::clean_names()
df_candidatos_2024 <- read_csv2("data/candidatos2024.csv")
df_prefs_eleitos_2020 <- read_csv2("data/prefs20_eleitos.csv")


# Formatando os CPFs no formato XXX.XXX.XXX-XX
df_prefs_eleitos_2020$cpf <- sub("([0-9]{3})([0-9]{3})([0-9]{3})([0-9]{2})", "\\1.\\2.\\3-\\4", df_prefs_eleitos_2020$cpf)
df_multas$cpf <- df_multas$cpf_cnpj


# Supondo que os dataframes já estejam carregados no ambiente
df_candidatos_2024$titulo_eleitoral <- df_candidatos_2024$NR_TITULO_ELEITORAL_CANDIDATO

# Combine os dataframes usando a coluna titulo_eleitoral como chave
df_combined <- merge(df_prefs_eleitos_2020, df_candidatos_2024, by = "titulo_eleitoral", all.x = TRUE)

# Verifique quais CPFs em df_multas estão presentes em df_combined
df_multas$presente_no_combined <- df_multas$cpf %in% df_prefs_eleitos_2020$cpf


status_indesejados <- c(
  "Baixado – Defesa deferida", 
  "Baixado – Recurso de ofício indeferido", 
  "Baixado – Recurso deferido", 
  "Baixado c/base na Lei 9.873/99 (Prescrito)", 
  "Baixado por determinação judicial", 
  "Baixado por prescrição intercorrente durante anál. Jurídica", 
  "Cancelado", 
  "Cancelado na homologação (AI sem defesa)", 
  "Substituição de multa por advertência", 
  "Substituído na homologação por outro"
)


df_multas <- df_multas %>% 
  filter(!situacao_debito %in% status_indesejados) %>%
  filter(as.Date(data_auto, format = "%d/%m/%Y") >= as.Date("2015-01-01"))



df_multas_reeleicao <- df_multas |> filter(presente_no_combined==TRUE)
# A nova coluna 'presente_no_combined' será TRUE se o CPF estiver presente em df_combined, caso contrário, será FALSE
df_multas_reeleicao <- df_multas_reeleicao %>%
  filter(!is.na(cpf_cnpj))
