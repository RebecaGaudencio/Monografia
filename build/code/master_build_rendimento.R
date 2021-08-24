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

lista_ano <- c("PNADC_2016_visita1.txt")

lista_chave <- c("input_PNADC_2016_visita1.txt")

basededados <- PNADcIBGE::read_pnadc(microdata = lista_ano, input_txt = lista_chave)
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


lista_ano <-  c("PNADC_2016_visita1",
                "PNADC_2016_visita5",
               "PNADC_2017_visita1",
               "PNADC_2017_visita5",
               "PNADC_2018_visita1",
               "PNADC_2018_visita5",
               "PNADC_2019_visita1",
               "PNADC_2019_visita5"
               )


lista_chave <- c("input_PNADC_2016_visita1",
                 "input_PNADC_2016_visita5",
                 "input_PNADC_2017_visita1",
                 "input_PNADC_2017_visita5",
                 "input_PNADC_2018_visita1",
                 "input_PNADC_2018_visita5",
                 "input_PNADC_2019_visita1",
                 "input_PNADC_2019_visita5"
                 )


 for (yr in lista) {

 setwd(in_dir)

 lista_pnad <- list.files(pattern = "PNADC_")

 chave_input <- list.files(pattern = "input_PNADC_")


basededados <- PNADcIBGE::read_pnadc(microdata = lista_pnad, input_txt = chave_input)
basededados2 <- pnadc_design(basededados)
##############################################
#   Declarando a variável de peso amostral   #
##############################################

populacao <- basededados %>%
  select(UF, Trimestre, Ano, V1032) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(populacao = mean(aux))


pop <- basededados %>%
  select(UF, Trimestre, Ano, V1032) %>%
  group_by(Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(pop = mean(aux))

######################################################
#   Rendimento Habitual Médio de Todos os Trabalhos  #
######################################################

rendtrahabit <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD4019) %>%
  group_by(UF, Ano) %>%
  mutate(aux = (V1032*VD4019), 
         aux1 = sum(aux, na.rm = TRUE)) %>%
  summarise(rendtrabhabit = mean(aux1))

######################################################
#   Rendimento Efetivo Médio de Todos os Trabalhos   #
######################################################

rendtrabefet <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD4020) %>%
  group_by(UF, Ano) %>%
  mutate(aux = (V1032*VD4020),
         aux1 = sum(aux, na.rm = TRUE)) %>%
  summarise(rendtrabeft = mean(aux1))


######################################################
#   Rendimento Efetivo Medio de Todas as Fontes     #
######################################################

rendtrabefetfontes <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD4022) %>%
  group_by(UF, Ano) %>%
  mutate(aux = (V1032*VD4022),
         aux1 = sum(aux, na.rm = TRUE)) %>%
  summarise(rendtrabeft = mean(aux1))


#####################################################
#        Rendimento Domiciliar per capita           #
#####################################################

rendpc <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5011) %>%
  group_by(UF, Ano) %>%
  mutate(aux = (V1032*VD5011),
         aux1 = sum(aux, na.rm = TRUE)) %>%
  summarise(rendpc = mean(aux1))


rendatotal <- basededados %>%
  select(UF, Trimestre, Ano, V1032, VD5011) %>%
  group_by(Ano) %>%
  mutate(aux = (V1032*VD5011),
         aux1 = sum(aux, na.rm = TRUE)) %>%
  summarise(rendpc = mean(aux1))




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


#############################################
#     Rendimento de Programas Sociais      #
#############################################

# BPC
BPC <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5001A) %>%
  dplyr::filter(V5001A == 1) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(BPC = mean(aux))

# Bolsa Familia
BF <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5002A) %>%
  dplyr::filter(V5002A == 1) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(BF = mean(aux))

# Outros Programas Sociais
PSocial <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5003A) %>%
  dplyr::filter(V5003A == 1) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(PSocial = mean(aux))


#############################################
#             Outros Rendimentos            #
#############################################

#Seguro desemprego
Segdesemprego <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5005A) %>%
  dplyr::filter(V5005A == 1) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(Segdesemprego = mean(aux)) 

#    Seguro Desemprego por Regiao    #
Segdesempregonorte <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5005A) %>%
  dplyr::filter(V5005A == 1 & (UF == "11" | UF == "12" | UF == "13"| UF == "14"| UF == "15"| UF == "16"| UF == "17")) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(Segdesempregonorte = mean(aux)) 

Segdesempregonordeste <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5005A) %>%
  dplyr::filter(V5005A == 1 & (UF == "21" | UF == "22" | UF == "23"| UF == "24"| UF == "25"| UF == "26"| UF == "27"| UF == "28"| UF == "29")) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(Segdesempregonordeste = mean(aux)) 

Segdesempregosudeste <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5005A) %>%
  dplyr::filter(V5005A == 1 & (UF == "31" | UF == "32" | UF == "33"| UF == "34"| UF == "35")) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(Segdesempregosudeste = mean(aux)) 

Segdesempregosul <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5005A) %>%
  dplyr::filter(V5005A == 1 & (UF == "41" | UF == "42" | UF == "43")) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(Segdesempregosul = mean(aux)) 

Segdesempregocentrooeste <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5005A) %>%
  dplyr::filter(V5005A == 1 & (UF == "50" | UF == "51" | UF == "52"| UF == "53")) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(Segdesempregocentrooeste = mean(aux)) 


#Aposentadoria
Aposentadoria <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5004A) %>%
  dplyr::filter(V5004A == 1) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(Aposentadoria = mean(aux)) 

#Doacao
Doacao <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5006A) %>%
  dplyr::filter(V5006A == 1) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(Doacao = mean(aux)) 

#Aluguel
Aluguel <- basededados %>%
  select(UF, Trimestre, Ano, V1032, V5007A) %>%
  dplyr::filter(V5007A == 1) %>%
  group_by(UF, Ano) %>%
  mutate(aux = sum(V1032)) %>%
  summarise(Aluguel = mean(aux)) 


####################################
#           Indice de Gini         #
####################################

basededados2 <- convey_prep(basededados2)

ginihab <- svyby(~VD4020, by = ~UF + Ano, basededados2, svygini, na.rm = TRUE)
colnames(ginihab) <- c ("UF", "GiniHab", "SE1")

giniefet <- svyby(~VD4019, by = ~UF + Ano , basededados2, svygini, na.rm = TRUE)
colnames(giniefet) <- c ("UF", "GiniEfet", "SE2")

######################################
#  Adicao de variaveis no dataframe  #
######################################
 
basededados <- basededados %>%
  mutate(domicilio = paste0(UPA, V1008))

#Fazer essa parte no analysis
renda_ajuda_gov <- basededados %>%
  group_by(Ano) %>%
  mutate (aux1 = sum(BPC), 
          aux2 = sum(BF), 
          aux3 = sum(PSocial), 
          aux4 = sum(Segdesemprego),
          ajudagoverno = (aux1+aux2+aux3+aux4)) %>%
  summarise(ajudagoverno = mean(ajudagoverno))


renda_extra <- basededados %>%
  group_by(Ano) %>%
  mutate (aux1 = sum(Aposentadoria), 
          aux2 = sum(Doacao), 
          aux3 = sum(Aluguel), 
          aux4 = sum(Segdesemprego),
          ajudaextra = (aux1+aux2+aux3+aux4)) %>%
  summarise(ajudaextra = mean(ajudaextra))

# Até esta linha fazer no analysis


#####################################
##     Concentracao de Renda       ##
#####################################

item <- basededados %>%
  group_by(Ano) %>%
  dplyr::arrange(desc(VD5011)) %>%
  select(VD5011, Ano, V1032) %>%
  mutate(aux1 = cumsum(V1032)) 

#################################
# Rendimento dos 1% mais ricos ##
##################################

item1 <- basededados %>%
  group_by(Ano) %>%
  dplyr::arrange(desc(VD5011)) %>%
  select(VD5011, Ano, V1032) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  dplyr:: filter(aux1 <= pop$pop*0.01) %>%
  mutate(aux2 = (VD5011*V1032)) %>%
  summarise(renda1rico = sum(aux2))

#################################
# Rendimento dos 5% mais ricos ##
##################################

item2 <- basededados %>%
  group_by(Ano) %>%
  dplyr::arrange(desc(VD5011)) %>%
  select(VD5011, Ano, V1032) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  dplyr:: filter(aux1 <= pop$pop*0.05) %>%
  mutate(aux2 = (VD5011*V1032)) %>%
  summarise(renda5rico = sum(aux2))

##################################
# Rendimento dos 10% mais ricos  #
##################################

item3 <- basededados %>%
  group_by(Ano) %>%
  dplyr::arrange(desc(VD5011)) %>%
  select(VD5011, Ano, V1032) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  dplyr:: filter(aux1 <= pop$pop*0.1) %>%
  mutate(aux2 = (VD5011*V1032)) %>%
  summarise(renda10rico = sum(aux2))
  
#######################################
#    Rendimento dos 50% seguintes     #
#######################################

item4 <- basededados %>%
  group_by(Ano) %>%
  dplyr::arrange(desc(VD5011)) %>%
  select(VD5011, Ano, V1032) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  dplyr:: filter(aux1 >= pop$pop*0.1 & aux1 <= pop$pop*0.6) %>%
  mutate(aux2 = (VD5011*V1032)) %>%
  summarise(renda50seguinte = sum(aux2))


#######################################
#    Rendimento dos 40% mais pobres   #
#######################################

item5 <- basededados %>%
  group_by(Ano) %>%
  dplyr::arrange((VD5011)) %>%
  select(VD5011, Ano, V1032) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  dplyr:: filter(aux1 <= pop$pop*0.4) %>%
  mutate(aux2 = (VD5011*V1032)) %>%
  summarise(renda40pobre = sum(aux2))

#######################################
#    Rendimento dos 40% mais pobres   #
#######################################

item6 <- basededados %>%
  group_by(Ano) %>%
  dplyr::arrange((VD5011)) %>%
  select(VD5011, Ano, V1032) %>%
  mutate(aux1 = cumsum(V1032)) %>%
  dplyr:: filter(aux1 <= pop$pop*0.5) %>%
  mutate(aux2 = (VD5011*V1032)) %>%
  summarise(renda50pobre = sum(aux2))

####################################
#       Rendimento dos Pobres      #    
####################################

#   Em 2020, o Banco Mundial considerava pobre toda pessoa que vivia com menos 
#   de US$5,50 por dia. Por mes, ao dolar equivalente a R$5,5, pobres no Brasil 
#   são aqueles que vivem com menos de R$907,5. 

itm <- basededados %>%
  group_by(UF,Ano) %>%
  dplyr::arrange(VD5011) %>%
  select(VD5011, Ano, V1032) %>%
  mutate(aux1 = cumsum(V1032),
         aux2 = (VD5011*V1032),
         aux3 = cumsum (aux2)) %>%
  dplyr:: filter(VD5011 < 907.5)



#######################################################
##                                                   ##        
#  Juncao de todas as variaveis num data frame unico  #
##                                                   ##
#######################################################

basefinal <- populacao
basefinal <- merge(basefinal, rendtrahabit, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, rendtrabefet, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, rendtrabefetfontes, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, rendtrahabit, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, rendpc, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, rendpc2, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa1rendhab, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa2rendhab, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa3rendhab, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa4rendhab, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa5rendhab, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa6rendhab, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa7rendhab, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa1rendeft, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa2rendeft, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa3rendeft, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa4rendeft, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa5rendeft, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa6rendeft, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, faixa7rendeft, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, BPC, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, BF, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PSocial, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, Segdesemprego, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, Segdesempregonorte, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, Segdesempregonordeste, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, Segdesempregosudeste, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, Segdesempregosul, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, Segdesempregocentrooeste, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, Aposentadoria, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, Doacao, by = c("UF","Ano"), all = TRUE) 
basefinal <- merge(basefinal, Aluguel, by = c("UF","Ano"), all = TRUE)
basefinal <- merge(basefinal, giniefet, by = c("UF","Ano"), all = TRUE)
basefinal <- merge(basefinal, ginihab, by = c("UF","Ano"), all = TRUE)


baserenda <- pop
baserenda <- merge(baserenda, rendatotal, by = c("Ano"), all = TRUE)
baserenda <- merge(baserenda, item1, by = c("Ano"), all = TRUE)
baserenda <- merge(baserenda, item2, by = c("Ano"), all = TRUE)
baserenda <- merge(baserenda, item3, by = c("Ano"), all = TRUE)
baserenda <- merge(baserenda, item4, by = c("Ano"), all = TRUE)
baserenda <- merge(baserenda, item5, by = c("Ano"), all = TRUE)
baserenda <- merge(baserenda, item6, by = c("Ano"), all = TRUE)


# Salvando data frame no excel

write.csv(basefinal, paste0("C:/Users/rebec/Desktop/Monografia/Monografia/build/output/DadosVisitas", yr , ".csv"))

write.csv(basefinal, paste0("C:/Users/rebec/Desktop/Monografia/Monografia/build/output/DadosRenda", yr , ".csv"))


}