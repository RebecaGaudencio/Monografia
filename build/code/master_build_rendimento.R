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

############################################################
#                          Folder Path                     #
############################################################

user <- Sys.info()[["user"]]
message(sprintf("Current User: %s\n"))
if (user == "rebec") {
  ROOT <- "C:/Users/rebec/Documents/GitHub/Monografia"
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

lista_ano <- c("PNADC_2016_visita1",
               "PNADC_2016_visita5",
               "PNADC_2017_visita1",
               "PNADC_2017_visita5",
               "PNADC_2018_visita1",
               "PNADC_2018_visita5",
               "PNADC_2019_visita1",
               "PNADC_2019_visita5")

lista_ano_trabalho <- c("PNADC_2016_visita5",
                        "PNADC_2017_visita5",
                        "PNADC_2018_visita5",
                        "PNADC_2019_visita5")

