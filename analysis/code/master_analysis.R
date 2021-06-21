# ##########################################################
##                   Chamando as bibliotecas              ##
############################################################

library(PNADcIBGE)
library(conflicted)
library(tidyverse)
library(magrittr)
library(haven)
library(hrbrthemes)
library(zoo)

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

#################################################################
##                 Loop p/ ler PNADCs em .csv                  ##
#################################################################

for (xx in lista_ano) {
  setwd(in_dir)
  db1 <- read.csv(paste0(in_dir, "/DadosBrutos", xx, ".csv"))
  basededados <- rbind(basededados, db1)
}

#################################################################
#                    Ajustes na base de dados                   #
#################################################################

basededados <- basededados %>%
  mutate(year = substr(basededados$year, 7,12))

basededados <- basededados %>%
  mutate(aa = substr(basededados$year, 1, 2),
         bb = substr(basededados$year, 3, 6),
         Tempo = paste0(bb,"-",aa))


basededados$Tempo <- as.yearqtr(basededados$Tempo)


###############################################################
#                   Criacao da Taxa de Desemprego             #
###############################################################


item1 <- basededados %>%
  group_by(Tempo) %>%
  mutate(aux1 = sum(desocup), aux2 = sum(workforce),
         taxadesemprego = (aux1/aux2)*100) %>%
  summarise(taxadesemprego = mean(taxadesemprego))


colnames(item2)[2] <- "taxadedesemprego"
colnames(item2)[1] <- "periodo"

ggplot(data = item1, aes(Tempo, taxadesemprego)) +
  geom_line(color = "gray20") + 
  geom_point(shape = 21, color = "black", fill = "indianred1", size = 3) +
  geom_vline(xintercept = item1$Tempo[33], linetype = 8) +
  theme_bw() +
  labs(x = "Trimestre",
       y = "Em %",
       title = "Evolução da Taxa de Desemprego no Brasil")


############################################################
#                   Desempregados por Raça                 #
############################################################


item1 <- basededados %>%
  mutate(taxadedesempregobrancos = (desocupcor1/ocupcor1)*100,
         taxadedesempregonegros = (desocupcor2/ocupcor2)*100,
         taxadedesempregoamarelos = (desocupcor3/ocupcor3)*100,
         taxadedesempregopardos = (desocupcor4/ocupcor4)*100,
         taxadedesempregoindios = (desocupcor5/ocupcor5)*100)


ggplot(item1, aes(x = Tempo, y = taxadedesempregobrancos)) +
  geom_line(aes(col = "Brancos"), size = 1) +
  geom_line(aes(y = taxadedesempregonegros , col = "Negros"), size = 1) +
  geom_line(aes(y = taxadedesempregoamarelos, col = "Amarelos"), size = 1) +
  geom_line(aes(y = taxadedesempregopardos, col = "Pardos"), size = 1) +
  geom_line(aes(y = taxadedesempregoindios, col = "Indígenas"), size = 1) +
  geom_vline(xintercept = item1$Tempo[865], linetype = 8) +
  theme_bw() +
  labs(x = "Trimestre",
       y = "Em %",
       title = "Evolução do Desemprego por Raça") +
  theme(legend.position = 'bottom')


############################################################
#               Desempregados por Escolaridade             #
############################################################


item1 <- basededados %>%
  mutate(taxadedesempregoesco1 = (desocupesco1/ocupesco1)*100,
         taxadedesempregoesco2 = (desocupesco2/ocupesco2)*100,
         taxadedesempregoesco3 = (desocupesco3/ocupesco3)*100,
         taxadedesempregoesco4 = (desocupesco4/ocupesco4)*100,
         taxadedesempregoesco5 = (desocupesco5/ocupesco5)*100,
         taxadedesempregoesco6 = (desocupesco5/ocupesco5)*100,
         taxadedesempregoesco7 = (desocupesco5/ocupesco5)*100)


ggplot(item1, aes(x = Tempo, y = taxadedesempregoesco1)) +
  geom_line(aes(col = "Sem Instrução"), size = 1) +
  geom_line(aes(y = taxadedesempregoesco2  , col = "Fundamental Incompleto"), size = 1) +
  geom_line(aes(y = taxadedesempregoesco3, col = "Fundamental Completo"), size = 1) +
  geom_line(aes(y = taxadedesempregoesco4, col = "Medio Incompleto"), size = 1) +
  geom_line(aes(y = taxadedesempregoesco5, col = "Medio Completo"), size = 1) +
  geom_line(aes(y = taxadedesempregoesco6, col = "Superior Incompleto"), size = 1) +
  geom_line(aes(y = taxadedesempregoesco7, col = "Superior Completo"), size = 1) +
    geom_vline(xintercept = item1$Tempo[865], linetype = 8) +
  theme_bw() +
  labs(x = "Trimestre",
       y = "Em %",
       title = "Evolução do Desemprego por Escolaridade") +
  theme(legend.position = 'bottom')