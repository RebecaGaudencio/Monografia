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


# Importacao dos dados e leitura da PNADc 
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


for (yr in lista_ano) {
  
  setwd(in_dir)
  
  lista_pnad <- list.files(pattern = yr)
  
  chave_input <- list.files(pattern = "input_PNADC_trimestral.sas")
  
  pnadc_df <- read_pnadc(microdata = lista_pnad, input_txt = chave_input)


# Populacao por estado (UF) #

populacao <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (populacao = mean(aux))


write.csv(populacao, paste0("C:/Users/rebec/Documents/GitHub/Monografia/build/tmp/populacao", "012012", ".csv"))


# 1. Forca de Trabalho por estado #
workforce <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4001) %>%
  dplyr::filter(VD4001 == 1) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (workforce = mean(aux))


# 2. Pop. Economicamente Ativa por estado # 
PEA <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 1 |VD4002 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (PEA = mean(aux))

# 3. Pop. em Ativa (>14 anos) por estado # 
PIA <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, V2009) %>%
  dplyr::filter(V2009 >= 14) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (PIA = mean(aux))


# 4. Ocupacao e Desocupacao  por estado # 
ocup <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 1) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocup = mean(aux))

desocup <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002) %>%
  dplyr::filter(VD4002 == 2) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocup = mean(aux))

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
  select(UF, Trimestre, Ano, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 1 & V2009 >= 14 & V2009 <= 19) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocup1 = mean(aux))



ocup2 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 1 & V2009 >= 20 & V2009 <= 24) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocup2 = mean(aux))


ocup3 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 1 & V2009 >= 25 & V2009 <= 59) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocup3 = mean(aux))


ocup4 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 1 & V2009 >= 60) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocup4 = mean(aux))


desocup1 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 2 & V2009 >= 14 & V2009 <= 19) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocup1 = mean(aux))


desocup2 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 2 & V2009 >= 20 & V2009 <= 24) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocup2 = mean(aux))


desocup3 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 2 & V2009 >= 25 & V2009 <= 59) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocup3 = mean(aux))


desocup4 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2009) %>%
  dplyr::filter(VD4002 == 2 & V2009 >= 60) %>%
  group_by(UF,Trimestre, Ano) %>%
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
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1 = mean(aux))


ocupesco2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2 = mean(aux))


ocupesco3 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3 = mean(aux))


ocupesco4 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4 = mean(aux))


ocupesco5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5 = mean(aux))


ocupesco6 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6) %>%
  group_by(UF, Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6 = mean(aux))


ocupesco7 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7 = mean(aux))

desocupesco1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1 = mean(aux))


desocupesco2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2 = mean(aux))


desocupesco3 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3 = mean(aux))


desocupesco4 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4 = mean(aux))


desocupesco5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5 = mean(aux))


desocupesco6 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6 = mean(aux))


desocupesco7 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7) %>%
  group_by(UF,Trimestre,Ano) %>%
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
  select(UF, Trimestre, Ano, V1028, VD4002, V2007) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupmasc = mean(aux))


ocupfem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 2) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupfem = mean(aux))


desocupmasc <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 1) %>%
  group_by(UF, Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupmasc = mean(aux))


desocupfem <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
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
  select(UF, Trimestre, Ano, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupcor1 = mean(aux))


ocupcor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupcor2 = mean(aux))


ocupcor3 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2010 == 3) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupcor3 = mean(aux))


ocupcor4 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2010 == 4) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupcor4 = mean(aux))


ocupcor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupcor5 = mean(aux))


desocupcor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupcor1 = mean(aux))

desocupcor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupcor2 = mean(aux))

desocupcor3 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2010 == 3) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupcor3 = mean(aux))

desocupcor4 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2010 == 4) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupcor4 = mean(aux))

desocupcor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupcor5 = mean(aux))

##########################################################
#       Ocupacao e Desocupacao por Genero  e  Raca      ##
##                                                      ## 
##########################################################

# 1. Brancos

ocupmascor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 1 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupmascor1 = mean(aux))


ocupfemcor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 2 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupfemcor1 = mean(aux))


desocupmascor1 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 1 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupmascor1 = mean(aux))


desocupfemcor1 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 2 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupfemcor1 = mean(aux))


# 2. Negros 

ocupmascor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 1 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupmascor2 = mean(aux))


ocupfemcor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 2 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupfemcor2 = mean(aux))


desocupmascor2 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 1 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupmascor2 = mean(aux))


desocupfemcor2 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 2 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupfemcor2 = mean(aux))


# 3. Indigenas

ocupmascor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 1 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupmascor5 = mean(aux))


ocupfemcor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 1 & V2007 == 2 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupfemcor5 = mean(aux))


desocupmascor5 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 1 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupmascor5 = mean(aux))


desocupfemcor5 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, V2007, V2010) %>%
  dplyr::filter(VD4002 == 2 & V2007 == 2 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupfemcor5 = mean(aux))


##########################################################
#   Ocupacao e Desocupacao por Escolaridade e Genero    ##
##                                                      ## 
##########################################################


ocupesco1masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1masc = mean(aux))

ocupesco1fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1fem = mean(aux))

ocupesco2masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2masc = mean(aux))

ocupesco2fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2fem = mean(aux))

ocupesco3masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3masc = mean(aux))

ocupesco3fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3fem = mean(aux))

ocupesco4masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4masc = mean(aux))

ocupesco4fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4fem = mean(aux))

ocupesco5masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5masc = mean(aux))

ocupesco5fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5fem = mean(aux))

ocupesco6masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6masc = mean(aux))

ocupesco6fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6fem = mean(aux))

ocupesco7masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7masc = mean(aux))

ocupesco7fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7fem = mean(aux))


desocupesco1masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1masc = mean(aux))

desocupesco1fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1fem = mean(aux))

desocupesco2masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2masc = mean(aux))

desocupesco2fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2fem = mean(aux))

desocupesco3masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3masc = mean(aux))

desocupesco3fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3fem = mean(aux))

desocupesco4masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4masc = mean(aux))

desocupesco4fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4fem = mean(aux))

desocupesco5masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5masc = mean(aux))

desocupesco5fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5fem = mean(aux))


desocupesco6masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6masc = mean(aux))

desocupesco6fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6fem = mean(aux))


desocupesco7masc <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7 & V2007 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco7masc = mean(aux))

desocupesco7fem <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2007) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7 & V2007 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco7fem = mean(aux))


##########################################################
#    Ocupacao e Desocupacao por Escolaridade e Raca      #
##                                                      ## 
##########################################################

# 1. Brancos

ocupesco1cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1 & V2010 == 1) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1cor1 = mean(aux))


ocupesco2cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2cor1 = mean(aux))


ocupesco3cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3cor1 = mean(aux))


ocupesco4cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4cor1 = mean(aux))


ocupesco5cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5cor1 = mean(aux))


ocupesco6cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6cor1 = mean(aux))


ocupesco7cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7cor1 = mean(aux))


desocupesco1cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1cor1 = mean(aux))


desocupesco2cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2 & V2010 == 1) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2cor1 = mean(aux))


desocupesco3cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3cor1 = mean(aux))


desocupesco4cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4 & V2010 == 1) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4cor1 = mean(aux))


desocupesco5cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5cor1 = mean(aux))


desocupesco6cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6 & V2010 == 1) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6cor1 = mean(aux))


desocupesco7cor1 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7 & V2010 == 1) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco7cor1 = mean(aux))


# 2. Negros

ocupesco1cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1 & V2010 == 2) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1cor2 = mean(aux))


ocupesco2cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2cor2 = mean(aux))


ocupesco3cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3cor2 = mean(aux))


ocupesco4cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4cor2= mean(aux))


ocupesco5cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5 & V2010 == 2) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5cor2 = mean(aux))


ocupesco6cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6 & V2010 == 2) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6cor2 = mean(aux))


ocupesco7cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7cor2 = mean(aux))


desocupesco1cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1cor2 = mean(aux))


desocupesco2cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2 & V2010 == 2) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2cor2 = mean(aux))


desocupesco3cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3cor2 = mean(aux))


desocupesco4cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4cor2 = mean(aux))


desocupesco5cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5cor2 = mean(aux))


desocupesco6cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6cor2 = mean(aux))


desocupesco7cor2 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7 & V2010 == 2) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco7cor2 = mean(aux))


# 3. Indígenas

ocupesco1cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 1 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco1cor5 = mean(aux))


ocupesco2cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 2 & V2010 == 5) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco2cor5 = mean(aux))


ocupesco3cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 3 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco3cor5 = mean(aux))


ocupesco4cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 4 & V2010 == 5) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco4cor5 = mean(aux))


ocupesco5cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 5 & V2010 == 5) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco5cor5 = mean(aux))


ocupesco6cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 6 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco6cor5 = mean(aux))


ocupesco7cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 1 & VD3004 == 7 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (ocupesco7cor5 = mean(aux))


desocupesco1cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 1 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco1cor5 = mean(aux))


desocupesco2cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 2 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco2cor5 = mean(aux))


desocupesco3cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 3 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco3cor5 = mean(aux))


desocupesco4cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 4 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco4cor5 = mean(aux))


desocupesco5cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 5 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco5cor5 = mean(aux))


desocupesco6cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 6 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco6cor5 = mean(aux))


desocupesco7cor5 <-  pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4002, VD3004, V2010) %>%
  dplyr::filter(VD4002 == 2 & VD3004 == 7 & V2010 == 5) %>%
  group_by(UF,Trimestre,Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (desocupesco7cor5 = mean(aux))

# 5. No. de informais por estado #

informais <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4009, VD4012) %>%
  dplyr::filter(VD4009 == "02" |VD4009 == "04"| VD4009 == "06"| (VD4009 == "08" & VD4012 == 2) | (VD4009 == "09" & VD4012 == 2) | VD4009 == "10") %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (informais = mean(aux))

# 6. No. de formais por estado #

formais <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4009, VD4012) %>%
  dplyr::filter(VD4009 == "01" |VD4009 == "03"| VD4009 == "05"| VD4009 == "07"|(VD4009 == "08" & VD4012 == 1) | (VD4009 == "09" & VD4012 == 1)) %>%
  group_by(UF,Trimestre, Ano) %>%
  mutate(aux = sum(V1028)) %>%
  summarise (formais = mean(aux))

# 7. No. de desalentados por estado #

desalentados <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, VD4005) %>%
  dplyr::filter(VD4005 == 1) %>%
  group_by(UF,Trimestre, Ano) %>%
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

item1 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, V2009, V3002, V4074, V4074A, VD4001, VD4002, V4001, V4002) %>%
  dplyr::filter(VD4002 == 2)

item2 <- item1 %>%
  dplyr::filter(V3002 != 1 | V4074 != 6 | V4074A != 8)

item3 <- pnadc_df %>%
  select(UF, Trimestre, Ano, V1028, V2009, V3002, V4074, V4074A, VD4001, VD4002, V4001, V4002) %>%
  dplyr::filter(VD4001 == 2 & V3002 == 2)

nemnem <- rbind(item2, item3)

# 8. No. de nem-nem por estado #

nemnem <- nemnem %>% dplyr:: filter(V2009 >=14 & V2009 <= 25) %>% 
  group_by(UF, Trimestre, Ano) %>%
  mutate(aux = sum (V1028)) %>%
  summarise(nemnem = mean (aux))


# Juncao de todas as variaveis num data frame unico

basefinal <- populacao
basefinal <- merge(basefinal, workforce, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PEA, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, PIA, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocup, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocup, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocup1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocup2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocup3, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocup4, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocup, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocup1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocup2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocup3, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocup4, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco3, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco4, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco6, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco7, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco3, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco4, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco6, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco7, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupmasc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupfem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupmasc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupfem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupcor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupcor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupcor3, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupcor4, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupcor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupcor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupcor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupcor3, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupcor4, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupcor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupmascor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupfemcor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupmascor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupfemcor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupmascor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupfemcor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupmascor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupfemcor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupmascor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupfemcor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupmascor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupfemcor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco1masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco1fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco2masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco2fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco3masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco3fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco4masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco4fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco5masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco5fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco6masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco6fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco7masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco7fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco1masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco1fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco2masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco2fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco3masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco3fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco4masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco4fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco5masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco5fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco6masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco6fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco7masc, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco7fem, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco1cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco2cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco3cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco4cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco5cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco6cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco7cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco1cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco2cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco3cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco4cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco5cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco6cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco7cor1, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco1cor2, by = c("UF", "Trimestre","Ano"), all = TRUE)
basefinal <- merge(basefinal, ocupesco2cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco3cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco4cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco5cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco6cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco7cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco1cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco2cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco3cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco4cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco5cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco6cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco7cor2, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco1cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco2cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco3cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco4cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco5cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco6cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, ocupesco7cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco1cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco2cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco3cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco4cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco5cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco6cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desocupesco7cor5, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, informais, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, formais, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, desalentados, by = c("UF", "Trimestre","Ano"), all = TRUE) 
basefinal <- merge(basefinal, nemnem, by = c("UF","Trimestre", "Ano"), all = TRUE)
basefinal <- basefinal %>% mutate(year = yr)


# Salvando data frame no excel

write.csv(basefinal, paste0("C:/Users/rebec/Documents/GitHub/Monografia/build/output/DadosBrutos", yr , ".csv"))

}


####################################################
#                                                  #
#                       TAXAS                      #
#                                                  #
####################################################

# 1. Taxa de Desemprego

Txdesemprego <- (desocupados[,3]/workforce[,3])
