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

...
# Importacao dos dados e leitura da PNADc 

setwd(in_dir)

lista_ano <- c("PNADC_2016_visita1.txt")

lista_chave <- c("input_PNADC_2016_visita1.txt")



lista_ano <-  c("PNADC_2016_visita1.txt",
                "PNADC_2016_visita5.txt",
               "PNADC_2017_visita1.txt",
               "PNADC_2017_visita5.txt",
               "PNADC_2018_visita1.txt",
               "PNADC_2018_visita5.txt",
               "PNADC_2019_visita1.txt",
               "PNADC_2019_visita5.txt")

lista_ano_trabalho <- c("PNADC_2016_visita5",
                        "PNADC_2017_visita5",
                        "PNADC_2018_visita5",
                        "PNADC_2019_visita5")


lista_chave <- c("input_PNADC_2016_visita1.txt",
                 "input_PNADC_2016_visita5.txt",
                 "input_PNADC_2017_visita1.txt",
                 "input_PNADC_2017_visita5.txt",
                 "input_PNADC_2018_visita1.txt",
                 "input_PNADC_2018_visita5.txt",
                 "input_PNADC_2019_visita1.txt",
                 "input_PNADC_2019_visita5.txt",)


basededados <- PNADcIBGE::read_pnadc(microdata = lista_ano, input_txt = lista_chave)

##############################################
#   Declarando a variável de peso amostral   #
##############################################

populacao <- basededados %>%
  select(UF, Trimestre, Ano, V1032) %>%
  group_by(UF, Trimestre, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(populacao = mean(aux))

######################################################
#   Rendimento Habitual Médio de Todos os Trabalhos  #
######################################################

rendtrahabit <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD4019) %>%
  dplyr::filter(VD4019 == "Valor") %>%
  group_by(UF, Trimestre, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(rendtrabhabit = mean(aux))

######################################################
#   Rendimento Efetivo Médio de Todos os Trabalhos   #
######################################################

rendtrabefet <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD4020) %>%
  dplyr::filter(VD4020 == "Valor") %>%
  group_by(UF, Trimestre, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(rendtrabeft = mean(aux))


#####################################################
#        Rendimento Domiciliar per capita           #
#####################################################

rendpc <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5011) %>%
  dplyr::filter(VD5011 == "Valor") %>%
  group_by(UF, Trimestre, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(rendpc = mean(aux))


