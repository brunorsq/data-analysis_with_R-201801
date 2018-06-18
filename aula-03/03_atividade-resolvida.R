### Atividade prática

## Vamos começar carregando o arquivo de dados preparado para esta aula
library(tidyverse)

salarios <- read_csv("aula-03/data/201802_dados_salarios_servidores.csv.gz")

### 1 ####
## 
## O arquivo possui 2 colunas de Remuneração, uma em Reais e outra em Dólares. 
## Crie uma nova coluna de Remuneração Final que terá a soma entre a remuneração em Reais e as remuneração em Dólares convertida para Reais.
## Atenção: Para conversão, utilize a Cotação Comercial do Dólar no último dia útil de Fevereiro.
## Após criar esta coluna, descarte todos os registros cuja Remuneração Final for menor que R$ 900,00
## 
### # ####
Sys.setlocale(locale = "pt_BR.UTF-8")

# EXPLICACAO: Cotação comercial para compra no dia 28/02/2018, consultado em https://economia.uol.com.br/cotacoes/cambio/dolar-comercial-estados-unidos/?historico
cambio_compra_fevereiro <- 3.2421

salarios %>%
  mutate( remuneracao_final = REMUNERACAO_REAIS + ( cambio_compra_fevereiro * REMUNERACAO_DOLARES )) %>%
  filter(remuneracao_final >= 900) -> salarios_em_reais

### 2 ####
## 
## Neste dataset é possível identificar que alguns servidores estão lotados em órgãos diferentes do seu órgão de exercício.
## Identifique os 5 cargos com maior quantidade de servidores que estão lotados em um órgão diferente, listando a descrição do cargo e a quantidade de servidores por cargo.
## Além de listar os 5 cargos e as quantidades, crie um vetor com os nomes destes 5 cargos. Crie este vetor com o nome de cargos_diferente_lotacao.
## 
## Dica: a função pull() do dplyr extrai uma variável em formato de vetor.
salarios %>% count(UF_EXERCICIO) %>% pull(UF_EXERCICIO) -> ufs # EXEMPLO
## 
### # ####

# EXPLICACAO: 
# - coloquei entre parênteses para criar o data frame, atribuir a uma variável e listar o resultado
# - top_n tem efeito similar à ordenação por n `arrange( desc( n ))` combinado com head `head( 5 )`
#   - a principal diferença está na ordenação do resultado.
( salarios_em_reais %>%
    filter( ORGSUP_LOTACAO != ORGSUP_EXERCICIO ) %>%
    count( DESCRICAO_CARGO ) %>%
    top_n( 5, n ) -> servidores_lotacao_diferente_exercicio )

cargos_diferente_lotacao <- pull( servidores_lotacao_diferente_exercicio, DESCRICAO_CARGO )

### 3 ####
## 
## Utilizando o vetor criado na atividade anterior, calcule a média e o desvio padrão de cada cargo, 
## separando entre aqueles que estão lotados em outro órgão e aqueles que estão lotados no mesmo órgão de exercício.
## O resultado deve conter:
##    - a descrição do cargo, 
##    - uma variável indicando se o servidor está lotado no mesmo órgão de exercício ou em um órgão diferente, 
##    - a média salarial
##    - o desvio padrão
##    - a mediana
##    - o desvio absoluto da mediana
##    - o menor salário
##    - o maior salário
## Analise os valores por lotação dentro de um mesmo cargo e comente ao final do exercício se você considera alguma diferença significativa.
## 
## Dica 1: o operador %in% testa se valores de uma variável pertencem ao conjunto de valores de um vetor. Lembre que deve ser utilizada a variável cargos_diferente_lotacao
salarios %>% filter(DESCRICAO_CARGO %in% c("MINISTRO DE PRIMEIRA CLASSE", "ANALISTA DE TEC DA INFORMACAO", "PESQUISADOR")) %>% count(DESCRICAO_CARGO) # EXEMPLO
## Dica 2: Será necessário agrupar (group_by) por mais de uma variável para calcular as estatísticas solicitadas. 
## A função group_by permite múltiplos nomes de variáveis na mesma chamada.
## 
### # ####

# EXPLICACAO: 
# - Criei a função mad (median absolute deviation) para facilitar o uso do summarise
# - Selecionando as 3 variáveis de interesse e agrupando pelas duas solicitadas foi possível usar o summarise_all enumerando todas as funções.
# - No final, uma forma alternativa de mostrar os resultados para comparação

mad <- function( x, na.rm = FALSE) {
  median( abs( x - median( x, na.rm )), na.rm )
}

( salarios_em_reais %>%
    filter( DESCRICAO_CARGO %in% cargos_diferente_lotacao ) %>%
    mutate( lotacao_orgsup = if_else( ORGSUP_LOTACAO != ORGSUP_EXERCICIO, "Outro", "Mesmo" )) %>%
    select( DESCRICAO_CARGO, lotacao_orgsup, remuneracao_final ) %>%
    group_by( DESCRICAO_CARGO, lotacao_orgsup ) %>%
    summarise_all( funs( mean, sd, median, mad, min, max )) %>%
    ungroup() -> estatisticas_descritivas )

estatisticas_descritivas %>%
  gather(key = estatistica, value = valor, -DESCRICAO_CARGO, -lotacao_orgsup ) %>%
  unite(col = estatistica_orgao, estatistica, lotacao_orgsup, sep = "_") %>%
  spread( estatistica_orgao, valor ) %>% 
  View()

