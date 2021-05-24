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


# Populacao por estado (UF) #

populacao <- pnadc_df %>%
  select(UF, Trimestre, V1028) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (populacao = mean(aux))


############################################################
#                                                          #   
##                Variaveis DO Mercado De Trabalho        ## 
#                                                          #
############################################################


# Ocupacao por estado # 

ocupacao <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupacao = mean(aux))


# Pop. Economicamente Ativa por estado # 

PEA <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 1 |VD4002 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (PEA = mean(aux))

# Pop. em Ativa (>14 anos) por estado # 

PIA <- pnadc_df %>%
  select(UF, Trimestre, V1028, V2009) %>%
  dplyr::filter(V2009 >= 14) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (PIA = mean(aux))


# No. de informais por estado #

informais <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4009, VD4012) %>%
  dplyr::filter(VD4009 == 2 |VD4009 == 4| VD4009 == 6| (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2) | VD4009 == 10) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (informais = mean(aux))

# No. de formais por estado #

formais <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4009, VD4012) %>%
  dplyr::filter(VD4009 == 1 |VD4009 == 3| VD4009 == 5| VD4009 == 7|(VD4009 == 8 & VD4012 == 1) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (formais = mean(aux))

# No. de desocupados por estado #

desocupados <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupados = mean(aux))


# No. de desalentados por estado #

desalentados <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4005) %>%
  dplyr::filter(VD4005 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desalentados = mean(aux))

# No. de nem-nem por estado #
nemnem <- pnadc_df %>%
  select(UF, Trimestre, V2009, V3002, V4074, V4074A, VD4001, VD4002) %>%
  dplyr::filter(VD4002 == 2| (VD4001 == 2 & VD3002 == 2)) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028), aux2 = ifelse(.pnadc_df$V4074 == 6)) %>%
  summarise (nemnem = mean(aux), teste =  mean(aux2))

# testar o codigo acima



