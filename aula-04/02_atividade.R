library(tidyverse)
library(lubridate)

## Nesta atividade você deve utilizar o resultado do exercício 01 da Atividade da aula 03 (remuneração em dólares convertida para reais)
## Utilize o código daquele exercício como ponto de partida para esta atividade. 
## Sempre utilize o caminho relativo, não o caminho absoluto, pois não funcionará na correção do exercício.

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

salarios %>%
  group_by(DESCRICAO_CARGO) %>%
  filter(n() >= 200) %>%
  summarise(correlacao = cor( x = year(DATA_INGRESSO_ORGAO), y = year(DATA_DIPLOMA_INGRESSO_SERVICOPUBLICO)),
            pos_neg = ifelse(correlacao > 0,"Positiva","Negativa"),
            grau = if(correlacao <= 0.3){
                      "Desprezível"
                    }else if(correlacao <= 0.5){
                      "Fraca"
                    }else if(correlacao <= 0.7){
                      "Moderada"
                    }else if(correlacao <= 0.9){
                      "Forte"
                    }else{
                      "Muito Forte"
                    }
            ) %>%
  ungroup() %>%
  select (DESCRICAO_CARGO,correlacao,pos_neg,grau) -> cargo_correlacao

cargo_correlacao

### 2 ###
##
## - A partir do dataset do exercício anterior, selecione os 10 cargos de correlação mais forte (seja positiva ou negativa) e os 
##   10 cargos de correlação mais fraca (de novo, independente de ser positiva ou negativa)
## - Para estes 20 cargos, determine a Moda do órgão de lotação (ORGSUP_LOTACAO) e de exercício (ORGSUP_EXERCICIO)
## - Reponda se existe diferença entre as modas e se existe relação entre a Força da Correlação e a diferença entre as modas 
##   (caso haja diferença)
##
### # ###

cargo_correlacao %>%
  arrange(desc(abs(correlacao)))%>%
  head(10) -> cor_forte

paste("10 cargos com correlação mais forte: ",paste(c(cor_forte$DESCRICAO_CARGO),collapse = ", "))

cargo_correlacao %>%
  arrange(abs(correlacao))%>%
  head(10) -> cor_fraca

paste("10 cargos com correlação mais fraca: ",paste(c(cor_fraca$DESCRICAO_CARGO),collapse = ", "))

cargo_minmax_cor <- merge(cor_fraca,cor_forte,all = TRUE)%>%
  arrange(desc(abs(correlacao)))

salarios %>%
  filter(DESCRICAO_CARGO %in% cargo_minmax_cor$DESCRICAO_CARGO) %>%
  group_by(DESCRICAO_CARGO,ORGSUP_LOTACAO) %>%
  summarise(quantidade = n()) %>%
  arrange(desc(quantidade)) %>%
  head(1) %>%
  ungroup() -> moda_orgsuplotacao

moda_orgsuplotacao
  
salarios %>%
  filter(DESCRICAO_CARGO %in% cargo_minmax_cor$DESCRICAO_CARGO) %>%
  group_by(DESCRICAO_CARGO,ORGSUP_EXERCICIO) %>%
  summarise(quantidade = n()) %>%
  arrange(desc(quantidade)) %>%
  head(1) %>%
  ungroup() -> moda_orgsupexercicio

moda_orgsupexercicio


paste("Moda do orgao de lotacao: ", moda_orgsuplotacao$DESCRICAO_CARGO)
paste("Moda do orgao de exercicio: ", moda_orgsupexercicio$DESCRICAO_CARGO)

print("Analise: Nao identifiquei diferenca entre as modas, em ambas o resultado foi AGENTE DE SAUDE PUBLICA")

