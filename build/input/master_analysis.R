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

home_dir <- file.path(ROOT, "analysis")
in_dir <- file.path(ROOT, "build", "output")
out_dir <- file.path(ROOT, "analysis", "output")
tmp_dir <- file.path(ROOT, "analysis", "tmp")
code_dir <- file.path(ROOT, "analysis", "code")


# Importacao dos dados e leitura da PNADc 
help("get_pnadc")

lista_ano <- c("PNADC_012012",
               "PNADC_022012",
               "PNADC_032012",
               "PNADC_042012")

lista_ano <- c("PNADC_012012",
               "PNADC_022012",
               "PNADC_032012",
               "PNADC_042012",
               "PNADC_012013",
               "PNADC_022013",
               "PNADC_032013",
               "PNADC_042013",
               "PNADC_012014",
               "PNADC_022014",
               "PNADC_032014",
               "PNADC_042014",
               "PNADC_012015",
               "PNADC_022015",
               "PNADC_032015",
               "PNADC_042015",
               "PNADC_012016",
               "PNADC_022016",
               "PNADC_032016",
               "PNADC_042016",
               "PNADC_012017",
               "PNADC_022017",
               "PNADC_032017",
               "PNADC_042017",
               "PNADC_012018",
               "PNADC_022018",
               "PNADC_032018",
               "PNADC_042018",
               "PNADC_012019",
               "PNADC_022019",
               "PNADC_032019",
               "PNADC_042019",
               "PNADC_012020",
               "PNADC_022020",
               "PNADC_032020",
               "PNADC_042020",
               "PNADC_012021"
               )

basededados <- data.frame( UF = character(),Trimestre = character(),Ano = character())


for (xx in lista_ano) {
  db1 <- read.csv(paste0(in_dir, "/DadosBrutos", xx, ".csv"))
  basededados <- rbind(basededados, db1)
}


basefinal <- merge(basefinal, workforce, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PEA, by = c("UF", "Trimestre","Ano"), all = TRUE)


