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


# Importacao dos dados e leitura da PNADc 

lista <- c("2016_visita1")

lista <- c("2016_visita1",
           "2017_visita1",
           "2018_visita1",
           "2019_visita1"
)

for (yr in lista) {
  
  setwd(in_dir)

  lista_pnad <- list.files(pattern = paste("dados_PNADC_", yr, sep = ""))
  
  chave_input <- list.files(pattern = paste("input_PNADC_" , yr, sep = ""))
  
  
basededados <- PNADcIBGE::read_pnadc(microdata = lista_pnad, input_txt = chave_input)
basededados <- PNADcIBGE::pnadc_deflator(data_pnadc = basededados, deflator.file = "deflator_PNADC_2019.xls")

##########################################################
#  Incluindo Linhas de Pobreza e Extrema Pobreza no DF   #
##########################################################

basededados <- basededados %>%
  mutate(LinhaPobreza = (1.66*5.5*30*1.57088710))


basededados <- basededados %>%
  mutate(LinhaExtremaPobreza = (1.66*1.9*30*1.57088710)) 
                                     
###############################################
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
  mutate(aux = (VD5011*CO2),
         aux1 = sum(aux, na.rm = TRUE)) %>%
  summarise(rendadompc = mean(aux1))

#############################################################################
#                                   1. POBREZA                              #
#############################################################################

PobrezaBrasil <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaBrasil = sum(V1032))

(sum(PobrezaBrasil$PobrezaBrasil)/sum(populacao$populacao))*100

###############################################
#             Probreza por Região             #
###############################################

PobrezaNorte <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(UF =="11" | UF == "12" | UF == "13"| UF == "14"| UF == "15"| UF == "16"| UF == "17") %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaNorte = sum(V1032))


PobrezaNordeste <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(UF == "21" | UF == "22" | UF == "23"| UF == "24"| UF == "25"| UF == "26"| UF == "27"| UF == "28"| UF == "29") %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaNordeste = sum(V1032))

PobrezaSudeste <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(UF == "31" | UF == "32" | UF == "33"| UF == "35") %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaSudeste = sum(V1032))

PobrezaSul <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(UF == "41" | UF == "42" | UF == "43") %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaSull = sum(V1032))

PobrezaCentroOeste <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(UF == "50" | UF == "51" | UF == "52"| UF == "53") %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaCentroOeste = sum(V1032))


##############################################
#            Pobreza por Sexo                #
##############################################

PobrezaMulher <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2007, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2007 == 2) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaMulher = sum(V1032))

PobrezaHomem <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2007, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2007 == 1) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaHomem = sum(V1032))

##############################################
#            Pobreza por Cor                #
##############################################

PobrezaPretos <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2010, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2010 == 2) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaPretos = sum(V1032))


PobrezaPardos <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2010, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2010 == 4) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaPardos = sum(V1032))


PobrezaBrancos <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2010, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2010 == 1) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaBrancos = sum(V1032))


#############################################
#        Pobreza por Sexo e Cor             #
#############################################

PobrezaMulherPreta <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2007, V2010, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2007 == 2 & V2010 == 2) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaMulherPreta = sum(V1032))


PobrezaMulherParda <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2007, V2010, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2007 == 2 & V2010 == 4) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaMulherParda = sum(V1032))

PobrezaMulherBranca <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2007, V2010, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2007 == 2 & V2010 == 1) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaMulherBranca = sum(V1032))



PobrezaHomemPreto <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2007, V2010, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2007 == 1 & V2010 == 2) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaHomemPreto = sum(V1032))

PobrezaHomemPardo <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2007, V2010, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2007 == 1 & V2010 == 4) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaHomemPardo = sum(V1032))

PobrezaHomemBranco <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2007, V2010, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2007 == 1 & V2010 == 1) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaHomemBranco = sum(V1032))


#############################################
#            Pobreza por Idade              #
#############################################

PobrezaGrupo1 <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2009, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2009>= 0 & V2009<= 14) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaGrupo1 = sum(V1032))

PobrezaGrupo2 <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2009, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2009>= 15 & V2009<= 29) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaGrupo2 = sum(V1032))

PobrezaGrupo3 <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2009, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2009>= 30 & V2009<= 59) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaGrupo3 = sum(V1032))

PobrezaGrupo4 <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V2009, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V2009>= 60) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(PobrezaGrupo4 = sum(V1032))


############################################################################
#                               2.EXTREMA POBREZA                          #
############################################################################


ExtremaPobrezaBrasil <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaExtremaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaExtremaPobreza) %>%
  summarise(ExtremaPobrezaBrasil = sum(V1032))

(sum(ExtremaPobrezaBrasil$ExtremaPobrezaBrasil)/sum(populacao$populacao))*100


###############################################
#         Extrema Pobreza por Região          #
###############################################

ExtremaPobrezaNorte <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaExtremaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(UF =="11" | UF == "12" | UF == "13"| UF == "14"| UF == "15"| UF == "16"| UF == "17") %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaExtremaPobreza) %>%
  summarise(ExtremaPobrezaNorte = sum(V1032))


ExtremaPobrezaNordeste <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaExtremaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(UF == "21" | UF == "22" | UF == "23"| UF == "24"| UF == "25"| UF == "26"| UF == "27"| UF == "28"| UF == "29") %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaExtremaPobreza) %>%
  summarise(ExtremaPobrezaNordeste = sum(V1032))


ExtremaPobrezaSudeste <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaExtremaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(UF == "31" | UF == "32" | UF == "33"| UF == "35") %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaExtremaPobreza) %>%
  summarise(ExtremaPobrezaSudeste = sum(V1032))


ExtremaPobrezaSul <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaExtremaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(UF == "41" | UF == "42" | UF == "43") %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaExtremaPobreza) %>%
  summarise(ExtremaPobrezaSull = sum(V1032))


ExtremaPobrezaCentroOeste <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaExtremaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(UF == "50" | UF == "51" | UF == "52"| UF == "53") %>%
  mutate(aux1 = cumsum(V1032)) %>%
  mutate(aux2 = (VD5011*V1032*CO2)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaExtremaPobreza) %>%
  summarise(ExtremaPobrezaCentroOeste = sum(V1032))


############################################################################
#                          3. COMPOSIÇÃO DA RENDA                          #
############################################################################

 #################################################
 #         Decompor a renda em 5 grupos:         # 
 #           1. Trabalho                         #
 #           2. Ajuda do Governo - Sem Auxílio   #
 #           3. Auxílio Emergencial              #
 #           4. Aposentadoria ou Pensão          #
 #           5. Doação                           #
 #################################################


RendaPobres <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  mutate(aux1 = (VD5011*V1032*CO2)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  summarise(RendaPobres = sum(aux1))


RendaPobresTrabalho <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza, VD4019) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  mutate(aux1 = (VD4019*CO2*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  summarise(RendaPobresTrabalho = sum(aux1, na.rm = TRUE))

(sum(RendaPobresTrabalho$RendaPobresTrabalho)/sum(RendaPobres$RendaPobres))*100

RendaPobresBPC <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, CO2e, LinhaPobreza, V5001A, V5001A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5001A == 1) %>%
  mutate(aux1 = (V5001A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  summarise(RendaPobresBPC = sum(aux1, na.rm = TRUE))

RendaPobresBF <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, CO2e, LinhaPobreza, V5002A, V5002A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5002A == 1) %>%
  mutate(aux1 = (V5002A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  summarise(RendaPobresBF = sum(aux1, na.rm = TRUE))

RendaPobresPSocial <-  basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, CO2e, LinhaPobreza, V5003A,V5003A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5003A == 1) %>%
  mutate(aux1 = (V5003A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  summarise(RendaPobresPSocial = sum(aux1, na.rm = TRUE))

RendaPobresSegdesemprego <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza, V5005A, V5005A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5005A == 1) %>%
  mutate(aux1 = (V5005A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  summarise(RendaPobresSegdesemprego = sum(aux1, na.rm = TRUE))

RendaPobresAposentadoria <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza, V5004A, V5004A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5004A == 1) %>%
  mutate(aux1 = (V5004A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  summarise(RendaPobresAposentadoria = sum(aux1, na.rm = TRUE))

RendaPobresDoacao <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza, V5006A, V5006A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5006A == 1) %>%
  mutate(aux1 = (V5006A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  summarise(RendaPobresDoacao = sum(aux1, na.rm = TRUE))

RendaPobresAluguel <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza, V5007A, V5007A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5007A == 1) %>%
  mutate(aux1 = (V5007A2*CO2e*V1032)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(RendaPobresAluguel = sum(aux1, na.rm = TRUE))

###############################################################
###############################################################
###############################################################

RendaTrabalho <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, VD4019) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  mutate(aux1 = (VD4019*CO2)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  summarise(RendaTrabalho = sum(aux1, na.rm = TRUE))

(sum(RendaTrabalho$RendaTrabalho)/sum(rendadompc$rendadompc))*100

RendaBPC <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, CO2e, V5001A, V5001A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5001A == 1) %>%
  mutate(aux1 = (V5001A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  summarise(RendaBPC = sum(aux1, na.rm = TRUE))

RendaBF <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, CO2e, V5002A, V5002A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5002A == 1) %>%
  mutate(aux1 = (V5002A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  summarise(RendaBF = sum(aux1, na.rm = TRUE))

RendaPSocial <-  basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, CO2e, V5003A,V5003A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5003A == 1) %>%
  mutate(aux1 = (V5003A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  summarise(RendaPSocial = sum(aux1, na.rm = TRUE))

RendaSegdesemprego <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, V5005A, V5005A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5005A == 1) %>%
  mutate(aux1 = (V5005A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  summarise(RendaSegdesemprego = sum(aux1, na.rm = TRUE))

RendaAposentadoria <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza, V5004A, V5004A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5004A == 1) %>%
  mutate(aux1 = (V5004A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  summarise(RendaAposentadoria = sum(aux1, na.rm = TRUE))

RendaDoacao <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza, V5006A, V5006A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5006A == 1) %>%
  mutate(aux1 = (V5006A2*CO2e*V1032)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  summarise(RendaDoacao = sum(aux1, na.rm = TRUE))

RendaAluguel <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza, V5007A, V5007A2) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  dplyr::filter(V5007A == 1) %>%
  mutate(aux1 = (V5007A2*CO2e*V1032)) %>%
  mutate(aux3 = (VD5011*CO2)) %>%
  dplyr::filter(aux3 < LinhaPobreza) %>%
  summarise(RendaAluguel = sum(aux1, na.rm = TRUE))



#########################################################################
#                                4. HIATO                               # 
#########################################################################


#################################################
#               Hiatos de Renda                 #
#################################################

HiatoRenda <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  mutate(aux1 = (VD5011*V1032*CO2)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  mutate(aux3 = (LinhaPobreza - aux2)) %>%
  summarise(HiatoRenda = mean(aux3))


#################################################
#                 Hiato Agregado                # 
#################################################

HiatoAgregado <- basededados %>%
  select(VD5011, Trimestre, UF, Ano, V1032, CO2, LinhaPobreza) %>%
  group_by(UF,Trimestre,Ano) %>%
  dplyr::arrange(VD5011) %>%
  mutate(aux1 = (VD5011*V1032*CO2)) %>%
  mutate(aux2 = (VD5011*CO2)) %>%
  dplyr::filter(aux2 < LinhaPobreza) %>%
  mutate(aux3 = (LinhaPobreza - aux2)) %>%
  summarise(HiatoAgregado = sum(aux3))



#######################################################
##                                                   ##        
#  Juncao de todas as variaveis num data frame unico  #
##                                                   ##
#######################################################

basefinal <- populacao
basefinal <- merge(basefinal, PobrezaBrasil, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaNorte, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaNordeste, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaSudeste, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaSul, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaCentroOeste, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaMulher, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaHomem, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaPretos, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaPardos, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaBrancos, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaMulherPreta, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaMulherParda, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaMulherBranca, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaHomemPreto, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaHomemPardo, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaHomemBranco, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaGrupo1, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaGrupo2, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaGrupo3, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PobrezaGrupo4, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ExtremaPobrezaBrasil, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ExtremaPobrezaNorte, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ExtremaPobrezaNordeste, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ExtremaPobrezaSudeste, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ExtremaPobrezaSul, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ExtremaPobrezaCentroOeste, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, RendaPobres, by = c("UF","Trimestre","Ano"), all = TRUE)
basefinal <- merge(basefinal, RendaPobresBPC, by = c("UF","Trimestre","Ano"), all = TRUE)
basefinal <- merge(basefinal, RendaPobresBF, by = c("UF","Trimestre","Ano"), all = TRUE)
basefinal <- merge(basefinal, RendaPobresPSocial, by = c("UF","Trimestre","Ano"), all = TRUE)
basefinal <- merge(basefinal, RendaPobresSegdesemprego, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, RendaPobresAposentadoria, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, RendaPobresDoacao, by = c("UF","Trimestre","Ano"), all = TRUE)
basefinal <- merge(basefinal, RendaPobresAluguel, by = c("UF","Trimestre","Ano"), all = TRUE)
basefinal <- merge(basefinal, RendaBPC, by = c("UF","Trimestre","Ano"), all = TRUE)
basefinal <- merge(basefinal, RendaBF, by = c("UF","Trimestre","Ano"), all = TRUE)
basefinal <- merge(basefinal, RendaPSocial, by = c("UF","Trimestre","Ano"), all = TRUE)
basefinal <- merge(basefinal, RendaSegdesemprego, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, RendaAposentadoria, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, RendaDoacao, by = c("UF","Trimestre","Ano"), all = TRUE)
basefinal <- merge(basefinal, RendaAluguel, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, HiatoRenda, by = c("UF","Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, HiatoAgregado, by = c("UF","Trimestre","Ano"), all = TRUE) 


# Salvando data frame no excel

write.csv(basefinal, paste0("C:/Users/rebec/Desktop/Monografia/Monografia/build/output/DadosPobreza", yr , ".csv"))

}
