####################################################################
#####                                                          #####
##                           MONOGRAFIA                           ##
#####                                                          #####
####################################################################

# Chamando as bibliotecas
library(PNADcIBGE)
library(conflicted)
library(tidyverse)
library(magrittr)
library(survey)
library(convey)


############################################################
#                          Folder Path                     #
############################################################

user <- Sys.info()[["user"]]
message(sprintf("Current User: %s\n"))
if (user == "rebec") {
  ROOT <- "C:/Users/rebec/Desktop/Monografia/Monografia"
} else if (user == "f.cavalcanti") {
  ROOT <- "C:/Users/Francisco/Dropbox"
} else {
  stop("Invalid user")
}

home_dir <- file.path(ROOT, "build")
in_dir <- file.path(ROOT, "build", "input")
out_dir <- file.path(ROOT, "build", "output")
tmp_dir <- file.path(ROOT, "build", "tmp")
code_dir <- file.path(ROOT, "build", "code")


# Importacao dos dados e leitura da PNADc - teste

setwd(in_dir)

lista_ano <- c("dados_PNADC_2016_visita1.txt")

lista_chave <- c("input_PNADC_2016_visita1.txt")

basededados <- PNADcIBGE::read_pnadc(microdata = lista_ano, input_txt = lista_chave)
basededados <- PNADcIBGE::pnadc_deflator(data_pnadc = basededados, deflator.file = "deflator_PNADC_2019.xls")
basededados2 <- pnadc_design(basededados)


# Importacao dos dados e leitura da PNADc 

lista <- c("2016_visita1")

lista <- c("2016_visita1",
           "2016_visita5",
           "2017_visita1",
           "2017_visita5",
           "2018_visita1",
           "2018_visita5",
           "2019_visita1",
           "2019_visita5"
)

#for (yr in lista) {
#  
#  setwd(in_dir)
  
#  lista_pnad <- list.files(pattern = paste("dados_PNADC_", yr, sep = ""))
#  
#  chave_input <- list.files(pattern = paste("input_PNADC_" , yr, sep = ""))
#  
#  
#basededados <- PNADcIBGE::read_pnadc(microdata = lista_pnad, input_txt = chave_input)


  
##############################################
#   Declarando a variável de peso amostral   #
##############################################

populacao <- basededados %>%
  select(UF, Trimestre, Ano, V1032) %>%
  group_by(UF, Trimestre, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(populacao = mean(aux))


######################################################
#    Rendimento Domiciliar (habitual)per capita      #
######################################################

rendadompc <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5011,CO2) %>%
  group_by(UF, Trimestre, Ano) %>%
  mutate(aux = (V1032*VD5011*CO2),
         aux1 = sum(aux, na.rm = TRUE)) %>%
  summarise(rendadompc = mean(aux1))





