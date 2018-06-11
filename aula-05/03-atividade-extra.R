
library(tidyverse)
library(ggplot2)
library(lubridate)

ted_main %>%
  filter(between(year(film_date),2012,2017))%>%
  group_by(year(film_date))%>%
  summarise(quant = sum(views))-> dados

ggplot(dados, aes(x = dados$`year(film_date)`,y = dados$quant)) + geom_histogram(stat = "sum", bins = 40) +
  xlab("Anos") + ylab("Quantidade visualizacoes")
