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


# Populacao por estado (UF) #

populacao <- pnadc_df %>%
  select(UF, Trimestre, V1028) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (populacao = mean(aux))


############################################################
#                                                          #   
##                Variaveis DO Mercado De Trabalho        ## 
#                                                          #
############################################################

# Força de Trabalho por estado #
workforce <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4001) %>%
  dplyr::filter(VD4001 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (workforce = mean(aux))


# Ocupacao por estado # 

ocupacao <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupacao = mean(aux))

##########################################################
##              Ocupacao por Faixa Etaria               ##
#                                                        #
#        1. Adolescentes --> 14 a 19 anos                #
#        2. Jovens --> 20 a 24 anos                      #
#        3. Adultos --> 25 a 59 anos                     #
#        4. Idosos --> 60+                               #
##                                                      ## 
##########################################################


ocup1 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 1 & V2009 >= 14 & V2009 <= 19) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocup1 = mean(aux))



ocup2 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 1 & V2009 >= 20 & V2009 <= 24) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocup2 = mean(aux))


ocup3 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 1 & V2009 >= 25 & V2009 <= 59) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocup3 = mean(aux))


ocup4 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 1 & V2009 >= 60) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocup4 = mean(aux))

##########################################################
##             Ocupacao por Escolaridade                ##
#                                                        #
#        1. Sem instrução                                #
#        2. Fundamental incompleto ou eq                 #
#        3. Fundamental completo                         #
#        4. Medio incompleto                             #
#        5. Medio completo                               #
#        6. Superior incompleto                          #
#        7. Superior completo                            #
##                                                      ## 
##########################################################


ocupesco1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1 = mean(aux))


ocupesco2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2 = mean(aux))


ocupesco3 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3 = mean(aux))


ocupesco4 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4 = mean(aux))


ocupesco5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5 = mean(aux))


ocupesco6 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6 = mean(aux))


ocupesco7 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7 = mean(aux))


# Pop. Economicamente Ativa por estado # 

PEA <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 1 |VD4002 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (PEA = mean(aux))

# Pop. em Ativa (>14 anos) por estado # 

PIA <- pnadc_df %>%
  select(UF, Trimestre, V1028, V2009) %>%
  dplyr::filter(V2009 >= 14) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (PIA = mean(aux))


# No. de informais por estado #

informais <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4009, VD4012) %>%
  dplyr::filter(VD4009 == 2 |VD4009 == 4| VD4009 == 6| (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2) | VD4009 == 10) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (informais = mean(aux))

# No. de formais por estado #

formais <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4009, VD4012) %>%
  dplyr::filter(VD4009 == 1 |VD4009 == 3| VD4009 == 5| VD4009 == 7|(VD4009 == 8 & VD4012 == 1) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (formais = mean(aux))

# No. de desocupados por estado #

desocupados <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupados = mean(aux))

##########################################################
##            Desocupados por Faixa Etaria              ##
#                                                        #
#        1. Adolescentes --> 14 a 19 anos                #
#        2. Jovens --> 20 a 24 anos                      #
#        3. Adultos --> 25 a 59 anos                     #
#        4. Idosos --> 60+                               #
##                                                      ## 
##########################################################

desocup1 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 2 & V2009 >= 14 & V2009 <= 19) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocup1 = mean(aux))



desocup2 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 2 & V2009 >= 20 & V2009 <= 24) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocup2 = mean(aux))


desocup3 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 2 & V2009 >= 25 & V2009 <= 59) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocup3 = mean(aux))


desocup4 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 2 & V2009 >= 60) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocup4 = mean(aux))

##########################################################
##            Desocupacao por Escolaridade              ##
#                                                        #
#        1. Sem instrução                                #
#        2. Fundamental incompleto ou eq                 #
#        3. Fundamental completo                         #
#        4. Medio incompleto                             #
#        5. Medio completo                               #
#        6. Superior incompleto                          #
#        7. Superior completo                            #
##                                                      ## 
##########################################################

desocupesco1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1 = mean(aux))


desocupesco2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2 = mean(aux))


desocupesco3 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3 = mean(aux))


desocupesco4 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4 = mean(aux))


desocupesco5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5 = mean(aux))


desocupesco6 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6 = mean(aux))


desocupesco7 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco7 = mean(aux))



# No. de desalentados por estado #

desalentados <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4005) %>%
  dplyr::filter(VD4005 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desalentados = mean(aux))

###################################################
#            No. de nem-nem por estado            #
#                                                 #
#   v2009:  Jovem de 14 a 25 anos                 #
#   VD4002: Não está ocupado                      #
#   V4001:  Não estagiou                          #
#   V3002:  Não frequenta a escola                #
#   V4002:  Não trabalhou nem estagiou na semana  #
#   VD4001: Fora da Força de Trabalho             #
#                                                 #
###################################################

nemnem <- pnadc_df %>%
  select(UF, Trimestre, V1028, V2009, V3002, V4074, VD4001, VD4002, V4001, V4002) %>%
  dplyr::filter(VD4002 == 2 &  VD4001 == 2 & V2009>=14 & V2009<=25 & VD3002 == 2 & V4001 ==2 & V4002 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028), aux2 = ifelse(.pnadc_df$V4074 == 6)) %>%
  summarise (nemnem = mean(aux), teste =  mean(aux2))

# testar o codigo acima

####################################################
#                                                  #
#            1. Taxa de Desemprego                 #
#                                                  #
####################################################

Txdesemprego <- (desocupados/workforce)*100
