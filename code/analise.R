library(tidyverse)

df_multas <- read_csv("multas1308.csv") |> janitor::clean_names()
df_candidatos_2024 <- read_csv2("candidatos2024_08_13.csv")
df_candidatos_2020 <- read_csv2("todos_candidatos_2020_com_cpf.csv")
df_prefs_eleitos_2020 <- read_csv2("prefs20_eleitos.csv")


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
  filter(as.Date(data_auto, format = "%d/%m/%Y") >= as.Date("2015-01-01"))%>%
  mutate(valor_do_auto = as.numeric(valor_do_auto))



# Função para remover acentos, cedilhas e converter para maiúsculas
remove_acentos <- function(text) {
  # Remover acentos e cedilhas
  text <- stri_trans_general(text, "Latin-ASCII")
  # Converter para maiúsculas
  text <- toupper(text)
  return(text)
}
# Exemplo de aplicação da função em um dataframe
# Aplicar a função à coluna 'nome'
df_multas$nome <- sapply(df_multas$nome_ou_razao_social, remove_acentos)

df_multas_reeleicao <- df_multas |> filter(presente_no_combined==TRUE)
# A nova coluna 'presente_no_combined' será TRUE se o CPF estiver presente em df_combined, caso contrário, será FALSE
df_multas_reeleicao <- df_multas_reeleicao %>%
  filter(!is.na(cpf_cnpj))
# O dataframe df_combined agora contém todas as colunas de df_prefs_eleitos_2020 mais as colunas correspondentes de df_candidatos_2024

# Remova o ponto dos valores para que sejam interpretados corretamente como números
df_multas$valor_corrigido <- as.numeric(gsub("\\.", "", df_multas$valor_do_auto))

# Verificar os valores corrigidos
head(df_multas$valor_corrigido)
df <- df_multas |> group_by(cpf_cnpj, nome) |> summarise(ocor =sum(valor_corrigido)) |> arrange(desc(ocor))
# Verifique os resultados
head(df_multas$valor_corrigido)

df_eleitos <-read_csv2("prefs20_eleitos.csv")

write_csv2(prefs20_eleitos2, "prefs20_eleitos_com_cpf.csv")
write_csv2(df_candidatos, "todos_candidatos_2020_com_cpf.csv")
write_csv2(df_multas_reeleicao, "df_multas_reeleicao.csv")


normaliza_texto <- function(texto) {
  # Coloca em maiúscula
  texto_maiusculo <- toupper(texto)
  
  # Substitui caracteres acentuados
  texto_normalizado <- gsub("Á|À|Ã|Â|Ä", "A", texto_maiusculo)
  texto_normalizado <- gsub("É|È|Ê|Ë", "E", texto_normalizado)
  texto_normalizado <- gsub("Í|Ì|Î|Ï", "I", texto_normalizado)
  texto_normalizado <- gsub("Ó|Ò|Õ|Ô|Ö", "O", texto_normalizado)
  texto_normalizado <- gsub("Ú|Ù|Û|Ü", "U", texto_normalizado)
  texto_normalizado <- gsub("Ç", "C", texto_normalizado)
  texto_normalizado <- gsub("Ñ", "N", texto_normalizado)
  
  return(texto_normalizado)
}

df_candidatos_2024$nome <- normaliza_texto(df_candidatos_2024$NM_CANDIDATO)
df_cpfs <- df_multas[nchar(df_multas$cpf_cnpj) == 14, ]

df_multas$nome <- normaliza_texto(df_multas$nome_ou_razao_social)


df_candidatos_2024$multado <- df_candidatos_2024$nome %in% df_cpfs$nome


candidatos24_multados <- df_candidatos_2024 |> filter(multado==TRUE)
candidatos24_multados <- merge(candidatos24_multados, df_candidatos_2020[, c("titulo_eleitoral", "cpf")], by = "titulo_eleitoral", all.x = TRUE)


write_csv2(candidatos24_multados,"candidatos24_multados_08_09.csv")