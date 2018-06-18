library(tidyverse)
library(lubridate)

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")

Sys.setlocale(locale = "pt_BR.UTF-8")

# EXPLICACAO: Cotação comercial para compra no dia 28/02/2018, consultado em https://economia.uol.com.br/cotacoes/cambio/dolar-comercial-estados-unidos/?historico
cambio_compra_fevereiro <- 3.2421

salarios %>%
  mutate( remuneracao_final = REMUNERACAO_REAIS + ( cambio_compra_fevereiro * REMUNERACAO_DOLARES )) %>%
  filter(remuneracao_final >= 900) -> salarios_em_reais

### IMPORTANTE ###
## Se você utilizar alguma função própria ou do material de aula, o código da(s) função(ões) deve estar neste arquivo da atividade.


### 1 ####
## 
## Correlação de ano de ingresso por cargo
## - Determine o coeficiente de correlação entre o tempo em anos desde a DATA_INGRESSO_ORGAO e o tempo em anos desde a DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO
##   para todos os cargos que possuem no mínimo 200 servidores.
## - Crie uma coluna que determina se a correlação é positiva ou negativa, e outra coluna que define a força da correlação de acordo com 
##   o material visto em aula sobre interpretação do coeficiente.
## - O resultado desta atividade deve ser um Data Frame com as variáveis de Cargo, Coeficiente de Correlação, Direção da Correlação e Força da Correlação
## 
### # ####

este_ano <- year( today() )

# EXPLICACAO: 
# - Uso de função para determinar a força da correlação

cor_strength <- function(x) {
  ax <- abs( x )
  case_when( ax <= 0.3 ~ "desprezível"
             , ax <= 0.5 ~ "fraca"
             , ax <= 0.7 ~ "moderada"
             , ax <= 0.9 ~ "forte"
             , TRUE      ~ "muito forte" )
}

( salarios_em_reais %>%
    mutate( tempo_orgao = este_ano - year( DATA_INGRESSO_ORGAO )
            , tempo_servico = este_ano - year( DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO )) %>%
    group_by( DESCRICAO_CARGO ) %>%
    filter( n() >= 200 ) %>%
    summarise( coef_cor_tempo = cor( tempo_orgao, tempo_servico )) %>%
    ungroup() %>%
    mutate( direcao_cor = if_else( coef_cor_tempo >= 0, "POSITIVA", "NEGATIVA" )
            , forca_cor   = cor_strength( coef_cor_tempo )) -> correlacao_tempo_cargos )

### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###

# EXPLICACAO: 
# - Não há uma função no `base R` para moda. A função mode não é a moda.
# - Grupo dos extremos usando union_all
# - semi_join para filtro 
# - junção dos data frames com inner_join

Moda <- function( x ) {
  ux <- unique( x )
  ux[ which.max( tabulate( match( x, ux ))) ]
}


union_all( correlacao_tempo_cargos %>%
             top_n( 10, abs( coef_cor_tempo ))
           , correlacao_tempo_cargos %>%
             top_n( 10, desc( abs( coef_cor_tempo )))) -> extremos_correlacoes

salarios_em_reais %>%
  semi_join( extremos_correlacoes, by = "DESCRICAO_CARGO" ) %>%
  group_by( DESCRICAO_CARGO ) %>%
  summarise( moda_lotacao = Moda( ORGSUP_LOTACAO ), moda_exercicio = Moda( ORGSUP_EXERCICIO ))  %>%
  ungroup() -> moda_cargos

correlacao_tempo_cargos %>%
  inner_join( moda_cargos, by = "DESCRICAO_CARGO") %>%
  arrange( forca_cor )
