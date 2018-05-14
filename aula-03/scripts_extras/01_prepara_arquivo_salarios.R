library(tidyverse)
library(lubridate)
library(magrittr)

Sys.setlocale(locale = "pt_BR")

cadastros <- read_tsv("aula-03/data/20180228_Cadastro.csv.gz", na = c("Não informada"), locale = locale(encoding = "latin1", date_names = "pt", date_format = "%d/%m/%y")) %>%
  mutate_if(is_character, ~ if_else(.x == "", NA_character_, .x)) %>%
  dplyr::select(Id_SERVIDOR_PORTAL, DESCRICAO_CARGO, ORG_LOTACAO, ORGSUP_LOTACAO, ORGSUP_EXERCICIO, SITUACAO_VINCULO, REGIME_JURIDICO, JORNADA_DE_TRABALHO, DATA_INGRESSO_ORGAO, DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO, UF_EXERCICIO) %>% 
  mutate_at(vars(DATA_INGRESSO_ORGAO, DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO), dmy) %>%
  rename(ID_SERVIDOR_PORTAL = Id_SERVIDOR_PORTAL)

cadastros %>%
  group_by(ID_SERVIDOR_PORTAL) %>%
  mutate( max_ocupacao = max(DATA_INGRESSO_ORGAO)
          , max_ingresso = max(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO)
          , DESCRICAO_CARGO = max(DESCRICAO_CARGO, na.rm = TRUE)
          , UF_EXERCICIO = max(UF_EXERCICIO, na.rm = TRUE)
          , SITUACAO_VINCULO = max(SITUACAO_VINCULO, na.rm = TRUE)
          , ORGSUP_EXERCICIO = max(ORGSUP_EXERCICIO, na.rm = TRUE)
          , JORNADA_DE_TRABALHO = max(JORNADA_DE_TRABALHO, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(DATA_INGRESSO_ORGAO == max_ocupacao & DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO == max_ingresso) %>%
  distinct() -> cadastros_unicos

salarios <- read_tsv("aula-03/data/20180228_Remuneracao.csv.gz", locale = locale(decimal_mark = ",")) %>%
  select(1:3, 6:7)

colnames(salarios) <- c("ANO", "MES", "ID_SERVIDOR_PORTAL", "REMUNERACAO_REAIS", "REMUNERACAO_DOLARES")

salarios %>% inner_join(cadastros_unicos, by="ID_SERVIDOR_PORTAL") %>% select(-starts_with("max")) -> dados_aula

colnames(dados_aula)

View(dados_aula)

write_csv(dados_aula, "aula-03/data/201802_dados_salarios_servidores.csv.gz")
