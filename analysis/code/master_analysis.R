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


#######################################################
#                   Taxa de Desemprego               #
#######################################################


item1 <- basededados %>%
  group_by(Tempo) %>%
  mutate(aux1 = sum(desocup), aux2 = sum(workforce),
         taxadesemprego = (aux1/aux2)*100) %>%
  summarise(taxadesemprego = mean(taxadesemprego))

ggplot(data = item1, aes(Tempo, taxadesemprego)) +
  geom_line(color = "gray20") + 
  geom_point(shape = 21, color = "black", fill = "indianred1", size = 3) +
  geom_vline(xintercept = item1$Tempo[32], linetype = 8) +
  theme_bw() +
  labs(x = "Trimestre",
       y = "Em %",
       title = "Evolução da Taxa de Desemprego no Brasil") +
  theme(plot.title = element_text(family = "Times"))


############################################################
#                   Desempregados por Raça                 #
############################################################

basededados[is.na(basededados)] <- 0 

item1 <- basededados %>%
    group_by(Tempo) %>%
    mutate(aux1 = sum(desocupcor1), aux2 = sum(ocupcor1), 
           aux3 = sum(desocupcor2), aux4 = sum(ocupcor2),
           aux5 = sum(desocupcor3), aux6 = sum(ocupcor3),
           aux7 = sum(desocupcor4), aux8 = sum(ocupcor4),
           aux9 = sum(desocupcor5), aux10 = sum(ocupcor5),
         taxadedesempregobrancos   = (aux1/(aux1+aux2))*100,
         taxadedesempregonegros = (aux3/(aux3+aux4))*100,
         taxadedesempregoamarelos = (aux5/(aux5+aux6))*100,
         taxadedesempregopardos = (aux7/(aux7+aux8))*100,
         taxadedesempregoindios = (aux9/(aux9+aux10))*100) %>%
  summarise(taxadedesempregobrancos = mean(taxadedesempregobrancos),
            taxadedesempregonegros = mean(taxadedesempregonegros),
            taxadedesempregoamarelos = mean(taxadedesempregoamarelos),
            taxadedesempregopardos = mean(taxadedesempregopardos),
            taxadedesempregoindios = mean(taxadedesempregoindios))


ggplot(item1, aes(x = Tempo, y = taxadedesempregobrancos)) +
  geom_line(aes(col = "Brancos"), color = "gray69", size = 1.2) +
  geom_line(aes(y = taxadedesempregonegros , col = "Negros"), color = "black", size = 1.2) +
  geom_line(aes(y = taxadedesempregoamarelos, col = "lightyellow4"), color = "yellow", size = 1.2) +
  geom_line(aes(y = taxadedesempregopardos, col = "Pardos"), color = "orange",size = 1.2) +
  geom_line(aes(y = taxadedesempregoindios, col = "Indígenas"),color = "indianred1",size = 1.2) +
  geom_vline(xintercept = item1$Tempo[32], linetype = 8) +
  theme_bw() +
  labs(x = "Trimestre",
       y = "Em %",
       title = "Evolução do Desemprego por Raça") +
  theme(legend.position = 'bottom') +
  theme(plot.title = element_text(family = "Times"))



############################################################
#               Desempregados por Escolaridade             #
############################################################

item1 <- basededados %>%
  group_by(Tempo) %>%
  mutate(aux1 = sum(desocupesco1), aux2 = sum(ocupesco1), 
         aux3 = sum(desocupesco2), aux4 = sum(ocupesco2),
         aux5 = sum(desocupesco3), aux6 = sum(ocupesco3),
         aux7 = sum(desocupesco4), aux8 = sum(ocupesco4),
         aux9 = sum(desocupesco5), aux10 = sum(ocupesco5),
         aux11 = sum(desocupesco6), aux12 = sum(ocupesco6),
         aux13 = sum(desocupesco7), aux14 = sum(ocupesco7),
         taxadedesempregoesco1 = (aux1/(aux1+aux2))*100,
         taxadedesempregoesco2 = (aux3/(aux3+aux4))*100,
         taxadedesempregoesco3 = (aux5/(aux5+aux6))*100,
         taxadedesempregoesco4 = (aux7/(aux7+aux8))*100,
         taxadedesempregoesco5 = (aux9/(aux9+aux10))*100,
         taxadedesempregoesco6 = (aux11/(aux11+aux12))*100,
         taxadedesempregoesco7 = (aux13/(aux13+aux14))*100) %>%
  summarise(taxadedesempregoesco1 = mean(taxadedesempregoesco1),
            taxadedesempregoesco2 = mean(taxadedesempregoesco2),
            taxadedesempregoesco3 = mean(taxadedesempregoesco3),
            taxadedesempregoesco4 = mean(taxadedesempregoesco4),
            taxadedesempregoesco5 = mean(taxadedesempregoesco5),
            taxadedesempregoesco6 = mean(taxadedesempregoesco6),
            taxadedesempregoesco7 = mean(taxadedesempregoesco7))



ggplot(item1, aes(x = Tempo, y = taxadedesempregoesco1)) +
  geom_line(aes(col = "Sem Instrução"), size = 1) +
  geom_line(aes(y = taxadedesempregoesco2  , col = "Fundamental Incompleto"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoesco3, col = "Fundamental Completo"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoesco4, col = "Medio Incompleto"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoesco5, col = "Medio Completo"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoesco6, col = "Superior Incompleto"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoesco7, col = "Superior Completo"), size = 1.1) +
    geom_vline(xintercept = item1$Tempo[32], linetype = 8) +
  theme_bw() +
  labs(x = "Trimestre",
       y = "Em %",
       title = "Evolução do Desemprego por Escolaridade") +
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(family = "Times"))


