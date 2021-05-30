####################################################################
#####                                                          #####
##                           MONOGRAFIA                           ##
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
##                VARIAVEIS DO MERCADO DE TRABALHO        ## 
#                                                          #
############################################################

# 1. Forca de Trabalho por estado #
workforce <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4001) %>%
  dplyr::filter(VD4001 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (workforce = mean(aux))


# 2. Ocupacao e Desocupacao  por estado # 
ocup <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocup = mean(aux))

desocup <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocups = mean(aux))

##########################################################
##       Ocupacao e Desocupacao por Faixa Etaria        ##
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
#        Ocupacao e Desocupacao por Escolaridade        ##
#                                                        #
#        1. Sem instrucao                                #
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

##########################################################
#        Ocupacao e Desocupacao por Genero              ##
#                                                        #
#        1. Homem                                        #
#        2. Mulher                                       #
##                                                      ## 
##########################################################


ocupmasc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupmasc = mean(aux))


ocupfem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupfem = mean(aux))


desocupmasc <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupmasc = mean(aux))


desocupfem <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupfem = mean(aux))

##########################################################
#        Ocupacao e Desocupacao por Raca                ##
#                                                        #
#        1. Branca                                       #
#        2. Preta                                        #
#        3. Amarela                                      #
#        4. Parda                                        #
#        5. Indigena                                     #          
##                                                      ## 
##########################################################

ocupcor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupcor1 = mean(aux))


ocupcor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupcor2 = mean(aux))


ocupcor3 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2010 == 3) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupcor3 = mean(aux))


ocupcor4 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2010 == 4) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupcor4 = mean(aux))


ocupcor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupcor5 = mean(aux))


desocupcor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupcor1 = mean(aux))

desocupcor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupcor2 = mean(aux))

desocupcor3 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2010 == 3) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupcor3 = mean(aux))

desocupcor4 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2010 == 4) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupcor4 = mean(aux))

desocupcor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupcor5 = mean(aux))

##########################################################
#       Ocupacao e Desocupacao por Genero  e  Raca      ##
##                                                      ## 
##########################################################

# 1. Brancos

ocupmascor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 1 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupmascor1 = mean(aux))


ocupfemcor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 2 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupfemcor1 = mean(aux))


desocupmascor1 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD2007 == 1 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupmascor1 = mean(aux))


desocupfemcor1 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 2 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupfemcor1 = mean(aux))


# 2. Negros 

ocupmascor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 1 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupmascor2 = mean(aux))


ocupfemcor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 2 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupfemcor2 = mean(aux))


desocupmascor2 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD2007 == 1 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupmascor2 = mean(aux))


desocupfemcor2 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 2 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupfemcor2 = mean(aux))


# 3. Indigenas

ocupmascor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 1 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupmascor5 = mean(aux))


ocupfemcor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 2 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupfemcor5 = mean(aux))


desocupmascor5 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD2007 == 1 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupmascor5 = mean(aux))


desocupfemcor5 <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 2 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupfemcor5 = mean(aux))


##########################################################
#   Ocupacao e Desocupacao por Escolaridade e Genero    ##
##                                                      ## 
##########################################################


ocupesco1masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1masc = mean(aux))

ocupesco1fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1fem = mean(aux))

ocupesco2masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2masc = mean(aux))

ocupesco2fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2fem = mean(aux))

ocupesco3masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3masc = mean(aux))

ocupesco3fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3fem = mean(aux))

ocupesco4masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4masc = mean(aux))

ocupesco4fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4fem = mean(aux))

ocupesco5masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5masc = mean(aux))

ocupesco5fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5fem = mean(aux))

ocupesco6masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6masc = mean(aux))

ocupesco6fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6fem = mean(aux))

ocupesco7masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7masc = mean(aux))

ocupesco7fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7fem = mean(aux))



desocupesco1masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1masc = mean(aux))

desocupesco1fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1fem = mean(aux))

desocupesco2masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2masc = mean(aux))

desocupesco2fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2fem = mean(aux))

desocupesco3masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3masc = mean(aux))

desocupesco3fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3fem = mean(aux))

desocupesco4masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4masc = mean(aux))

desocupesco4fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4fem = mean(aux))

desocupesco5masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5masc = mean(aux))

desocupesco5fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5fem = mean(aux))


desocupesco6masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6masc = mean(aux))

desocupesco6fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6fem = mean(aux))


desocupesco7masc <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7 & V2007 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco7masc = mean(aux))

desocupesco7fem <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7 & V2007 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco7fem = mean(aux))



##########################################################
#   Ocupacao e Desocupacao por Escolaridade e Raca    ##
##                                                      ## 
##########################################################

# 1. Brancos

ocupesco1cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1cor1 = mean(aux))


ocupesco2cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2cor1 = mean(aux))


ocupesco3cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3cor1 = mean(aux))


ocupesco4cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4cor1 = mean(aux))


ocupesco5cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5cor1 = mean(aux))


ocupesco6cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6cor1 = mean(aux))


ocupesco7cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7cor1 = mean(aux))


desocupesco1cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1cor1 = mean(aux))


desocupesco2cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2cor1 = mean(aux))


desocupesco3cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3cor1 = mean(aux))


desocupesco4cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4cor1 = mean(aux))


desocupesco5cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5cor1 = mean(aux))


desocupesco6cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6cor1 = mean(aux))


desocupesco7cor1 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7 & V2010 == 1) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco7cor1 = mean(aux))


# 2. Negros

ocupesco1cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1cor2 = mean(aux))


ocupesco2cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2cor2 = mean(aux))


ocupesco3cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3cor2 = mean(aux))


ocupesco4cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4cor2= mean(aux))


ocupesco5cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5cor2 = mean(aux))


ocupesco6cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6cor2 = mean(aux))


ocupesco7cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7cor2 = mean(aux))


desocupesco1cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1cor2 = mean(aux))


desocupesco2cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2cor2 = mean(aux))


desocupesco3cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3cor2 = mean(aux))


desocupesco4cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4cor2 = mean(aux))


desocupesco5cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5cor2 = mean(aux))


desocupesco6cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6cor2 = mean(aux))


desocupesco7cor2 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7 & V2010 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco7cor2 = mean(aux))


# 3. Indígenas

ocupesco1cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1cor5 = mean(aux))


ocupesco2cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2cor5 = mean(aux))


ocupesco3cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3cor5 = mean(aux))


ocupesco4cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4cor5 = mean(aux))


ocupesco5cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5cor5 = mean(aux))


ocupesco6cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6cor5 = mean(aux))


ocupesco7cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7cor5 = mean(aux))


desocupesco1cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1cor5 = mean(aux))


desocupesco2cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2cor5 = mean(aux))


desocupesco3cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3cor5 = mean(aux))


desocupesco4cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4cor5 = mean(aux))


desocupesco5cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5cor5 = mean(aux))


desocupesco6cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6cor5 = mean(aux))


desocupesco7cor5 <-  pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7 & V2010 == 5) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco7cor5 = mean(aux))




# 3. Pop. Economicamente Ativa por estado # 

PEA <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 1 |VD4002 == 2) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (PEA = mean(aux))

# 4. Pop. em Ativa (>14 anos) por estado # 

PIA <- pnadc_df %>%
  select(UF, Trimestre, V1028, V2009) %>%
  dplyr::filter(V2009 >= 14) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (PIA = mean(aux))


# 5. No. de informais por estado #

informais <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4009, VD4012) %>%
  dplyr::filter(VD4009 == 2 |VD4009 == 4| VD4009 == 6| (VD4009 == 8 & VD4012 == 2) | (VD4009 == 9 & VD4012 == 2) | VD4009 == 10) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (informais = mean(aux))

# 6. No. de formais por estado #

formais <- pnadc_df %>%
  select(UF, Trimestre, V1028, VD4009, VD4012) %>%
  dplyr::filter(VD4009 == 1 |VD4009 == 3| VD4009 == 5| VD4009 == 7|(VD4009 == 8 & VD4012 == 1) | (VD4009 == 9 & VD4012 == 2)) %>%
  group_by(UF,Trimestre) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (formais = mean(aux))

# 7. No. de desalentados por estado #

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
#                       TAXAS                      #
#                                                  #
####################################################

# 1. Taxa de Desemprego

Txdesemprego <- (desocupados[,3]/workforce[,3])
