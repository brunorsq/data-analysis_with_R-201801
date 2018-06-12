# biblioteca que implementa um Banco de Dados Relacional Tradicional, embutido (não executa um processo em separado)
# install.packages("RSQLite")

# Banco de Dados Relacional e Colunar de propósito analítico, embutido
# install.packages("MonetDBLite")

# Biblioteca para objetos JSON
# install.packages("jsonlite")

library(tidyverse)

# Carrega arquivo das TED Talks e corrige o campo URL. Converte campos de data
ted_talks <- read_csv("aula-08/data/ted_main.csv") %>% 
  mutate(url = str_replace(url, '\n', '')) %>%
  mutate( film_date = as.Date.POSIXct( film_date )
        , published_date = as.Date.POSIXct( published_date ))

# Cria data.frame (tibble) ted_main com os atributos principais de uma talk
ted_main <- ted_talks %>%
  select(url, name, main_speaker, speaker_occupation, num_speaker, title, event, duration, film_date, published_date, comments, languages, views, description)
  
# Cria data.frame (tibble) ted_ratings com cada categoria atribuida a uma ted_talk, a quantidade de vezes que a categoria foi votada e a proporção de votos de cada categoria
ted_ratings <- ted_talks %>%
  select( url, ratings ) %>%
  mutate( ratings = map( ratings, ~ jsonlite::fromJSON( str_replace_all( .x, "'", '"' )))) %>%
  unnest( ratings ) %>%
  select( -id ) %>%
  rename( category = name ) %>%
  filter( count > 0 ) %>%
  rename( count_ratings = count ) %>%
  group_by( url ) %>%
  mutate(rating_ratio = count_ratings / sum( count_ratings, na.rm = TRUE )) %>%
  ungroup()
  
library(DBI)
library(MonetDBLite)

# Estabelece conexão com MonetDBLite usando caminho previamente definido
dbdir <- "aula-08/data/monetdb/ted"
my_db <- MonetDBLite::src_monetdblite(dbdir)

# Cria tabela temporária com ted_ratings
tb_ted_ratings <- copy_to(my_db, df = ted_ratings, name = "ted_ratings_tmp", overwrite = TRUE, temporary = TRUE)

# Cria tabela temporária com ted_main
tb_ted_main <- copy_to(my_db, df = ted_main, name = "ted_main_tmp", overwrite = TRUE, temporary = TRUE)

# Para cada talk, determinar a categoria de rating mais comum (por ratio) e o total geral de tags que a talk recebeu
tb_ted_ratings %>%
  group_by( url ) %>%
  summarise( max_ratio = max( rating_ratio ), ratings = sum( count_ratings )) %>%
  ungroup() %>%
  inner_join( tb_ted_ratings, by=c("url", "max_ratio" = "rating_ratio" )) %>%
  select( url, category, ratings ) -> ted_defining_category

# Consulta em SQL
show_query( ted_defining_category )

tb_ted_main %>%
  inner_join( ted_defining_category, by = "url" ) -> tb_ted_main

show_query( tb_ted_main )

# Grava ted_main com novas colunas
tb_ted_main    <- copy_to( my_db, tb_ted_main, name = "ted_main", overwrite = TRUE, temporary = FALSE )
tb_ted_ratings <- copy_to( my_db, tb_ted_ratings, name = "ted_ratings", overwrite = TRUE, temporary = FALSE )

# Encerra conexão
MonetDBLite::monetdblite_shutdown()

# Remove data frames
rm(my_db, tb_ted_main, tb_ted_ratings, ted_defining_category, ted_main, ted_ratings, ted_talks)

# Nova conexão
dbdir <- "aula-08/data/monetdb/ted"
my_db_new_conn <- MonetDBLite::src_monetdblite(dbdir)

teds <- tbl( my_db_new_conn, "ted_main" )

head(teds, 10)

head(teds, 10) %>% show_query()

MonetDBLite::monetdblite_shutdown()
