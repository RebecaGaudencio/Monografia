####################################################################
#####                                                          #####
##                           Monografia                           ##
#####                                                          #####
####################################################################

# Pacotes necessarios
install.packages("usethis")
install.packages("PNADcIBGE")
install.packages("tidyverse")
install.packages("magrittr")


# Chamando as bibliotecas
library(PNADcIBGE)
library(conflicted)
library(tidyverse)
library(magrittr)

# Importacao dos dados e leitura da PNADc 
help("get_pnadc")

input_path <- file.path("C:/Users/rebec/Documents/GitHub/Monografia/build/input")
setwd(input_path)

lista_PNAD = list.files(pattern = "PNADC_012012.txt")
chave_input = list.files(pattern = "input_PNADC_trimestral.sas")


pnadc_df = read_pnadc(microdata=lista_PNAD, input_txt = chave_input)


pnadc_df %>% sum(V1028)

populacao <- pnadc_df %>%
  select(UF, Trimestre, V1028) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (populacao = mean(aux))

ocupacao <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupacao = mean(aux))


PEA <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 1 |VD4002 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (PEA = mean(aux))


PIA <- pnadc_df %>%
  select(UF, Trimestre, V1028, V2009) %>%
  dplyr::filter(V2009 >= 14) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (PIA = mean(aux))

informais <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4009, VD4012) %>%
  dplyr::filter(VD4009 == 2 |VD4009 == 4| VD4009 == 6| (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2) | VD4009 == 10) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (informais = mean(aux))




