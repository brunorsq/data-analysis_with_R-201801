library(tidyverse)

br_height <- 
  read_csv( "aula-07/data/Brazil.csv" ) %>% 
  rename( year = Year_of_birth, height = Mean_height, lo_95 = Mean_height_lower_95perc, hi_95 = Mean_height_upper_95perc ) %>%
  mutate( Sex = factor( Sex ))


# 1
br_height %>%
  filter( year == 1979, Sex == "Men" ) %>%
  select( height, lo_95, hi_95 ) %>%
  mutate( height_ratio = 181 / height, heigh_in_ic = between( 181, lo_95, hi_95 ))


# 2
media_anac_altura <- 173.1
sd_anac_altura <- 7.3

media_anac_idade <- 40
sd_anac_idade <- 12


# 3

pnorm( mean = media_anac_idade, sd = sd_anac_idade, q = 60, lower.tail = FALSE )
pnorm( mean = media_anac_idade, sd = sd_anac_idade, q = 20, lower.tail = TRUE  )

# 1.65 * sd_anac_idade ~~ 95%


# 4

menor_ano <- 2004 - 60
maior_ano <- 2008 - 20

br_height %>%
  filter( between( year, menor_ano, maior_ano )) %>%
  group_by( Sex ) %>%
  summarise( mean_height = mean( height )) %>%
  ungroup()


# 5

( 171 - media_anac_altura ) / sd_anac_idade
