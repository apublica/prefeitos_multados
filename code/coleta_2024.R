library(tidyverse)
library(httr)
library(lubridate)
library(janitor)
library(utils)

### Função para criar diretório com a data atual
create_date_dir <- function(base_dir) {
  date_dir <- file.path(base_dir, format(Sys.Date(), "%Y-%m-%d"))
  dir.create(date_dir, showWarnings = FALSE)
  return(date_dir)
}

### Função para baixar e extrair arquivos zip
download_and_extract_zip <- function(url, destfile, extract_dir) {
  GET(url, write_disk(destfile, overwrite = TRUE))
  dir.create(extract_dir, showWarnings = FALSE)
  unzip(destfile, exdir = extract_dir)
  cat("Arquivos extraídos para", extract_dir, "\n")
}

### Função para ler e converter arquivos CSV
read_and_convert_csv <- function(file) {
  df <- read_csv2(file, locale = locale(encoding = "latin1"))
  df <- mutate(df, SG_UE = as.character(SG_UE))
  return(df)
}
setwd("~/Documents/repos/eleicoes2024/prefeitos_multados")
### Diretório base
base_dir <- "data"

### Criar diretório com a data atual
date_dir <- create_date_dir(base_dir)

### Download e extração de arquivos zip
consulta_cand_url <- "https://cdn.tse.jus.br/estatistica/sead/odsele/consulta_cand/consulta_cand_2024.zip"
consulta_cand_destfile <- file.path(date_dir, "consulta_cand_2024.zip")
consulta_cand_extract_dir <- file.path(date_dir, "consulta_cand_2024_files")

download_and_extract_zip(consulta_cand_url, consulta_cand_destfile, consulta_cand_extract_dir)

file_list <- list.files(path = consulta_cand_extract_dir, pattern = "\\.csv$", full.names = TRUE)
candidatos2 <- file_list %>%
  lapply(read_and_convert_csv) %>%
  bind_rows() %>%
  distinct()


write_csv2(candidatos2, "data/candidatos2024.csv")

