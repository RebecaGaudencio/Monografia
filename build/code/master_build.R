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


