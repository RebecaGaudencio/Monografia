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
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(populacao = mean(aux))

######################################################
#   Rendimento Habitual Médio de Todos os Trabalhos  #
######################################################

rendtrahabit <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD4019) %>%
  dplyr::filter(VD4019 == "Valor") %>%
  group_by(UF, Ano) %>%
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

rendpc <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5011) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(rendpc = mean(aux))

######################################################
#     Faixa de Rendimento domiciliar per capita      #
#                      Habitual                      #
######################################################

#Faixa 1 
faixa1rendhab <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5012) %>%
  dplyr::filter(VD5012 == 1) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa1rendhab = mean(aux))

#Faixa 2 
faixa2rendhab <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5012) %>%
  dplyr::filter(VD5012 == 2) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa2rendhab = mean(aux))

#Faixa 3 
faixa3rendhab <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5012) %>%
  dplyr::filter(VD5012 == 3) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa3rendhab = mean(aux))

#Faixa 4 
faixa4rendhab <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5012) %>%
  dplyr::filter(VD5012 == 4) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa4rendhab = mean(aux))

#Faixa 5 
faixa5rendhab <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5012) %>%
  dplyr::filter(VD5012 == 5) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa5rendhab = mean(aux))

#Faixa 6 
faixa6rendhab <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5012) %>%
  dplyr::filter(VD5012 == 6) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa6rendhab = mean(aux))

#Faixa 7 
faixa7rendhab <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5012) %>%
  dplyr::filter(VD5012 == 7) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa7rendhab = mean(aux))


######################################################
#     Faixa de Rendimento domiciliar per capita      #
#                      Efetivo                       #
######################################################

#Faixa 1 
faixa1rendeft <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5006) %>%
  dplyr::filter(VD5006 == 1) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa1rendeft = mean(aux))

#Faixa 2 
faixa2rendeft <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5006) %>%
  dplyr::filter(VD5006 == 2) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa2rendeft = mean(aux))

#Faixa 3 
faixa3rendeft <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5006) %>%
  dplyr::filter(VD5006 == 3) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa3rendeft = mean(aux))

#Faixa 4 
faixa4rendeft <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5006) %>%
  dplyr::filter(VD5006 == 4) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa4rendeft = mean(aux))

#Faixa 5 
faixa1rendeft <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5006) %>%
  dplyr::filter(VD5006 == 5) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa5rendeft = mean(aux))

#Faixa 6 
faixa6rendeft <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5006) %>%
  dplyr::filter(VD5006 == 6) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa6rendeft = mean(aux))

#Faixa 7 
faixa7rendeft <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5006) %>%
  dplyr::filter(VD5006 == 7) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(faixa7endeft = mean(aux))
