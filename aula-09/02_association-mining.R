#' ---
#' title: "Association Mining"
#' output: html_notebook
#' ---
#' 
#' Importante: [Baixar dados do Instacart](https://www.instacart.com/datasets/grocery-shopping-2017)
#' Documentação dos arquivos: [link](https://gist.github.com/jeremystan/c3b39d947d9b88b3ccff3147dbcf6c6b)
#' 
#' Tutorial: [Support, Confidence, Lift](https://www.kdnuggets.com/2016/04/association-rules-apriori-algorithm-tutorial.html)
#' 
#' # Preparação dos dados
## ----message=FALSE, warning=FALSE----------------------------------------
library(tidyverse)

products <- read_csv("aula-09/data/instacart_2017_05_01/products.csv.gz")
orders <- read_csv("aula-09/data/instacart_2017_05_01/orders.csv.gz")
order_items <- read_csv("aula-09/data/instacart_2017_05_01/order_products__train.csv.gz")

order_items %>% 
  left_join(products,by="product_id")  ->
  transactioned_items

write.csv(transactioned_items, file = "aula-09/data/transactions.csv")

#' 
#' ## Estatísticas descritivas: transações
## ------------------------------------------------------------------------
# install.packages("arules")
library(arules)
transactioned_items <- read.transactions("aula-09/data/transactions.csv", format = "single", sep = ",",cols = c(2,6))

summary(transactioned_items)

#' 
#' # Exemplos de "carrinhos de compras"
## ------------------------------------------------------------------------
inspect(transactioned_items[1:3])

#' 
#' ## Mineração de padrões : regras de associação
## ------------------------------------------------------------------------
rules <- apriori(transactioned_items, parameter = list(support = 0.001, confidence = 0.15))

#' 
#' # Estatísticas descritivas: Regras aprendidas
## ------------------------------------------------------------------------
summary(rules)

#' 
#' # Regras com maior probabilidade condicional
## ------------------------------------------------------------------------
arules::inspect(sort(rules, by='confidence', decreasing = TRUE))[1:5]

#' 
#' # Regras com maior número de ocorrências (reparem que também é o maior suporte)
## ------------------------------------------------------------------------
inspect(sort(rules, by='count', decreasing = TRUE)[1:10])

#' 
