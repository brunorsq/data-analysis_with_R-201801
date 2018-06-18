# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse
library(tidyverse)

# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 
ted_talks <- read_csv("aula-05/data/ted_main.csv.gz")

# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado?
summary(ted_talks)

# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..
library(lubridate)
library(magrittr)

ted_talks %<>%
  mutate( duration  = duration(duration, units = "seconds")
          , film_date = as_datetime(film_date)
          , published_date = as_datetime(published_date))

# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation
ted_talks %<>%
  mutate( event = factor(event)
          , speaker_occupation = factor(speaker_occupation))

# Retire do dataframe a variável name
ted_talks %<>%
  select(-name)

# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas
summary(ted_talks)

# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.
ted_talks %<>%
  mutate(languages = if_else(languages == 0, 1L, languages))

# Verifique os 15 registros com menor data de filmagem. 
ted_talks %>%
  arrange(film_date) %>%
  head(15)

# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo
ted_talks %>%
  mutate(ano = year(film_date)) %>%
  count(ano) %>%
  as.data.frame()

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.
ted_talks %>%
  mutate(ano = year(film_date)) %>%
  count(ano) -> apresentacoes_por_ano

quantile(apresentacoes_por_ano$n, probs = seq(from=0, to=1, by=.1))

ted_talks %<>% 
  filter(year(film_date) >= 2005)

# Verifique novamente o resumo dos dados do dataframe
summary(ted_talks)

# Verifique os 10 registros com maior duração.
ted_talks %>%
  arrange(desc(duration)) %>%
  head(10)

# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas
ted_talks %>%
  filter(as.numeric(duration) > mean(duration) + 3 * sd(duration))

# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil
ted_talks %>%
  filter(duration > quantile(x = duration, probs = 0.75) + 1.5 * IQR(duration))

# Visualize os 10 quantis da quantidade de visualizações
quantile(ted_talks$views, probs = seq(from=0, to=1, by=.1))

# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?
mean(ted_talks$views)
median(ted_talks$views)

(MAD <- median( abs( ted_talks$views - median( ted_talks$views ))))
sd(ted_talks$views)

IQR(ted_talks$views)
MAD

IQR(ted_talks$views) / MAD

# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações
ted_talks %>%
  arrange(desc(views)) %>%
  select(languages) %>%
  head(244) %>%
  summarise_all(.funs = funs(mean, sd, median, IQR))

ted_talks %>%
  arrange(views) %>%
  select(languages) %>%
  head(244) %>%
  summarise_all(.funs = funs(mean, sd, median, IQR))

# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro
ted_talks %>%
  filter(str_detect(event, "TED")) %>%
  count(event)

# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES
ted_talks %>%
  filter(str_detect(event, "TED"), views > 1135710) %>%
  group_by(event) %>%
  filter(n() > 10) %>%
  summarise(n(), min(year(published_date)), mean(languages), sd(languages), sd(languages) / mean(languages)) %>%
  as.data.frame()

# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas

cor(ted_talks$views, ted_talks$languages)
cor(ted_talks$views, ted_talks$duration)
cor(ted_talks$views, ted_talks$comments)
cor(ted_talks$languages, ted_talks$comments)

# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas
media <- mean(ted_talks$duration)
desv_pad <- sd(ted_talks$duration)

ted_talks %>%
  filter(as.numeric(duration) <= media + 3 * desv_pad) ->
  ted_talks_mais_curtas

cor(ted_talks_mais_curtas$views, ted_talks_mais_curtas$languages)
cor(ted_talks_mais_curtas$views, ted_talks_mais_curtas$duration)
cor(ted_talks_mais_curtas$views, ted_talks_mais_curtas$comments)
cor(ted_talks_mais_curtas$languages, ted_talks_mais_curtas$comments)


# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado
ted_talks %>%
  mutate(ano_filmagem = year(film_date)) %>%
  group_by(ano_filmagem) %>%
  summarise(md_duracao = median(duration)) %>%
  ungroup() %$%
  cor(ano_filmagem, md_duracao)

ted_talks %>%
  mutate( ano_publicacao = year( as_datetime( film_date ))) %>%
  filter( between( ano_publicacao, 2012, 2017 )) %>%
  ggplot( aes(x = views)) +
  geom_histogram( bins = 1000 ) +
  facet_wrap( ~ ano_publicacao, ncol = 3 ) +
  theme_bw()
