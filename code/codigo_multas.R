library(tidyverse)
library(httr)
library(lubridate)
library(janitor)

# Multas

## Download
# Configuração das opções de segurança
set_config(config(ssl_verifypeer = FALSE))

# Lista de UFs
UFS <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MT","MS","MG","PA","PB","PE","PI","PR","RS","RJ","RR","RN","RO","SP","TO","SC","SE")

# Lista de URLs
urls <- sapply(UFS, function(uf) {
  paste0("https://dadosabertos.ibama.gov.br/dados/SICAFI/", uf, "/Quantidade/multasDistribuidasBensTutelados.csv")
})

# Importando os dados
df_list <- lapply(urls, function(url) {
  res <- GET(url)
  df <- read.csv(text = content(res, "text"), sep=";", encoding="UTF-8", stringsAsFactors = FALSE, na.strings = "NA")
  obj_name <- strsplit(url, "/")[[1]][5]
  names(df) <- gsub(" ", "_", names(df))
  assign(obj_name, df, envir = .GlobalEnv)
  return(df)
})

# Concatenando os dados
dados <- bind_rows(df_list)
dados <- dados |> 
  clean_names()

dados$data_auto <- as.Date(dados$data_auto, format = "%d/%m/%Y")

## Limpando e filtrando os dados de interesse

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


dados_de_interesse <- dados %>% 
  filter(!situacao_debito %in% status_indesejados) %>%
  filter(as.Date(data_auto, format = "%d/%m/%Y") >= as.Date("2015-01-01"))%>%
  mutate(valor_do_auto = as.numeric(valor_do_auto))
dados_de_interesse$moeda <- NULL
dados_de_interesse$ultima_atualizacao_relatorio <- NULL


# Função para remover acentos, cedilhas e converter para maiúsculas
remove_acentos <- function(text) {
  # Remover acentos e cedilhas
  text <- stri_trans_general(text, "Latin-ASCII")
  # Converter para maiúsculas
  text <- toupper(text)
  return(text)
}
# Exemplo de aplicação da função em um dataframe
dados_de_interesse$nome <- sapply(dados_de_interesse$nome_ou_razao_social, remove_acentos)

write_csv2(dados_de_interesse,"multas2024_08_13.csv")