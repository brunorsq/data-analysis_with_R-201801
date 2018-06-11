# Carregue a biblioteca tidyverse. Lembre que outras bibliotecas serão carregadas junto ao tidyverse

library(tidyverse)
library(tibble)
library(lubridate)

# Crie um dataframe com o conteúdo do arquivo ted_main.csv.gz. 

ted_main <- read_csv("aula-05/data/ted_main.csv.gz")


# Visualize o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas.
# As variáveis duration, film_date e published_date estão no tipo de dados apropriado? Não

str(ted_main)
summary(ted_main)


# Converta as seguintes variáveis utilizando o pacote Lubridate:
#     * duration, para duração (em segundos). Experimente utilizar as funções as.duration e duration. Mantenha aquela que considerar mais apropriada.
#     * film_date, para data, com a função as_datetime.
#     * published_date, para data, com a função as_datetime..

ted_main$duration <- duration(ted_main$duration)
ted_main$film_date <- as_datetime(ted_main$film_date)
ted_main$published_date <- as_datetime(ted_main$published_date)

# Converta as seguintes variáveis character para variáveis categóricas com a função factor.
#     * event
#     * speaker_occupation

ted_main$event <- factor(ted_main$event)
ted_main$speaker_occupation <- factor(ted_main$speaker_occupation)

# Retire do dataframe a variável name

ted_main$name <- NULL


# Visualize novamente o resumo dos dados do dataframe. Verifique os mínimos, máximos, médias e medianas das variáveis numéricas. Verifique as contagens das variáveis categóricas
str(ted_main)
summary(ted_main)


# Verifique quais registros possuem a menor quantidade de línguas. Corrija para que possuam no mínimo 1 idioma.

ted_main %>%
  mutate(languages = ifelse(languages == 0,1,languages))%>%
  arrange(languages)%>%
  head(10)


# Verifique os 15 registros com menor data de filmagem. 

ted_main %>%
  arrange(film_date)%>%
  head(15)


# Crie um dataframe com a contagem de apresentações por ano de filmagem e visualize todo o seu conteúdo

apresentacoes_ano <- ted_main %>%
                        group_by(year(film_date)) %>%
                        summarise(count = n())
View(apresentacoes_ano)

# Analise os 10 quantis da quantidade de apresentações por ano.
# Descarte, do data frame de apresentações do TED Talks, aqueles cujo ano de filmagem tiver quantidade de apresentações menor ou igual à quantidade do quarto quantil.


quantis_apresentacoes_ano <- quantile(apresentacoes_ano$count, probs = seq(0,1,0.1))
quantis_apresentacoes_ano

quarto_quartil <- quantis_apresentacoes_ano[5]

ted_main %>%
  group_by(year(film_date)) %>%
  summarise(count = n()) %>%
  filter(count<=quarto_quartil)-> anos_remover

ted_main %>%
  filter(!(year(film_date) %in% anos_remover$`year(film_date)`)) -> ted_main

# Verifique novamente o resumo dos dados do dataframe

str(ted_main)

# Verifique os 10 registros com maior duração.


ted_main %>%
  arrange(desc(ted_main$duration))%>%
  head(10)

# Existem apresentações com duração maior que 3 desvios padrão acima da média? Liste elas

media <- mean(ted_main$duration)
desvio_padrao <- sd(ted_main$duration)
tres_desvio_padrao <- media + (desvio_padrao*3)

ted_main%>%
  filter(duration > as.duration(tres_desvio_padrao))


# Calcule os 4 quartis e o IQR da duração das apresentações. Liste as apresentações cuja duração supera 1.5 * o IQR + o terceiro quartil

quartis <- quantile(as.numeric(ted_main$duration))
ted_main_iqr <- IQR(as.integer(ted_main$duration))

ted_main%>%
  filter(as.numeric(duration) > (1.5 * ted_main_iqr + quartis[4]))

# Visualize os 10 quantis da quantidade de visualizações


quantile(ted_main$views, probs = seq(0,1,0.1))

# Compare as seguintes estatísticas descritivas da quantidade de visualizações:
#   * Média e Mediana. Qual é maior?
#   * Desvio Absoluto da Mediana e Desvio Padrão. Qual é maior?
#   * Desvio Absoluto da Mediana e IQR. Quantas vezes o IQR é maior que o Desvio Absoluto da Mediana?
#   * Com base na média e na mediana, e na razão entre o IQR e o Desvio Absoluto da Mediana, 
#     você conclui que as quantidades de visualização estão distribuidas de forma simétrica em torno da média?

ted_main %>%
  summarise(
    media = mean(views),
    mediana = median(views),
    media_mediana = ifelse(media>mediana,"Media e maior","Mediana e maior"),
    dam = (median(abs(views - mediana))),
    dp = sd(views),
    iqr_views = IQR(views),
    dam_dp = ifelse(dam>dp,"Desvio absoluto e maior que o Desvio padrao","Desvio padrao e maior que o Desvio absoluto"),
    dam_iqr = str_c("O IQR e ",iqr_views/dam," vezes maior que o Desvio absoluto")
  )

print("Esta distribuido de forma simetrica porque o IQR corresponde ao dobro do Desvio Absoluto")


# Calcule a média, o desvio padrão, a mediana e o IQR da quantidade de línguas dos seguintes grupos:
#     * 10% de vídeos com maior número de visualizações
#     * 10% de vídeos com menor número de visualizações
quartis_views <- quantile(ted_main$views, probs = seq(0,1,0.1))

ted_main %>%
  filter(views >= as.integer(quartis_views[10]) || views <= as.integer(quartis_views[2])) %>%
  summarise(
    media = mean(languages),
    dp = sd(languages),
    mediana = median(languages),
    iqr_linguas = IQR(languages)
  )
  

# Determine a quantidade de apresentações por evento cujo nome inicie com TED. Utilize a função str_detect para este filtro

ted_main %>%
  filter(str_detect(event,"^TED")) %>%
  group_by(event)%>%
  summarise(quantidade = n())

# Determine, por evento cujo nome inicie com TED e que a quantidade de visualizações dos vídeos foi maior que a mediana calculada anteriormente.
#   * a quantidade de apresentações resultante do filtro, por evento
#   * o ano do evento (utilizar o menor ano da data de publicação)
#   * a quantidade média de línguas das apresentações
#   * o desvio padrão da quantidade de línguas
#   * o coeficiente de variação da quantidade de línguas
### EXIBA SOMENTE OS EVENTOS COM MAIS DE 10 APRESENTAÇÕES




# Calcule e classifique as seguintes correlações
#     * Quantidade de visualizações e Quantidade de línguas
#     * Quantidade de visualizações e Duração
#     * Quantidade de visualizações e Quantidade de Comentários
#     * Quantidade de Comentários e Quantidade de línguas




# Descarte os vídeos cuja duração seja maior que 3 desvios padrões da média. Calcule novamente as 5 correlações solicitadas




# Utilizando o data frame original, crie um dataframe com a mediana da duração dos vídeos por ano de filmagem. Calcule a correlação entre o ano e a mediana da duração
# e interprete o resultado




