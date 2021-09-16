###########################################################
##                   Chamando as bibliotecas             ##
###########################################################

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
  ROOT <- "C:/Users/rebec/Desktop/Monografia/Monografia"
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


# Importacao dos dados e leitura da PNADc Anual

lista_visita <- c("2016_visita1",
                  "2017_visita1",
                  "2018_visita1",
                  "2019_visita1"
                  )

basededados <- data.frame(UF = character(), Trimestre = character(), Ano = character())


#################################################################
##                 Loop p/ ler PNADCs em .csv                  ##
#################################################################

for (xx in lista_visita) {
  setwd(in_dir)
  db1 <- read.csv(paste0(in_dir, "/DadosPobreza", xx, ".csv"))
  basededados <- rbind(basededados, db1)
}



###########################################################
#              Pobreza e Extrema Pobreza                  #
###########################################################

basededados[is.na(basededados)] <- 0

item1 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(ExtremaPobrezaBrasil),
         aux2 = sum(ExtremaPobrezaNorte),
         aux3 = sum(ExtremaPobrezaNordeste),
         aux4 = sum(ExtremaPobrezaSudeste),
         aux5 = sum(ExtremaPobrezaSul),
         aux6 = sum(ExtremaPobrezaCentroOeste),
         aux7 = sum(populacao),
         aux8 = sum(popNorte),
         aux9 = sum(popNordeste),
         aux10 = sum(popSudeste),
         aux11 = sum(popSul),
         aux12 = sum(popCentroOeste),
         EPbrasil = ((aux1/aux7)*100),
         EPnorte = ((aux2/aux8)*100),
         EPnordeste = ((aux3/aux9)*100),
         EPsudeste = ((aux4/aux10)*100),
         EPsul = ((aux5/aux11)*100),
         EPcentrooeste = ((aux6/aux12)*100)) %>%
  summarise(EPbrasil = mean(EPbrasil),
            EPnorte = mean(EPnorte),
            EPnordeste = mean(EPnordeste),
            EPsudeste = mean(EPsudeste),
            EPsul = mean(EPsul),
            EPcentrooeste = mean(EPcentrooeste))

item1 <- t(item1)


item2 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(PobrezaBrasil),
         aux2 = sum(PobrezaNorte),
         aux3 = sum(PobrezaNordeste),
         aux4 = sum(PobrezaSudeste),
         aux5 = sum(PobrezaSul),
         aux6 = sum(PobrezaCentroOeste),
         aux7 = sum(populacao),
         aux8 = sum(popNorte),
         aux9 = sum(popNordeste),
         aux10 = sum(popSudeste),
         aux11 = sum(popSul),
         aux12 = sum(popCentroOeste),
         Pbrasil = ((aux1/aux7)*100),
         Pnorte = ((aux2/aux8)*100),
         Pnordeste = ((aux3/aux9)*100),
         Psudeste = ((aux4/aux10)*100),
         Psul = ((aux5/aux11)*100),
         Pcentrooeste = ((aux6/aux12)*100)) %>%
  summarise(Pbrasil = mean(Pbrasil),
            Pnorte = mean(Pnorte),
            Pnordeste = mean(Pnordeste),
            Psudeste = mean(Psudeste),
            Psul = mean(Psul),
            Pcentrooeste = mean(Pcentrooeste))

item2 <- t(item2)

item3 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(PobrezaHomem),
         aux2 = sum(Homens),
         aux3 = sum(PobrezaMulher),
         aux4 = sum(Mulheres),
         Pmulher = ((aux1/aux2)*100),
         Phomem = ((aux3/aux4)*100)) %>%
  summarise(Phomem = mean(Phomem),
            Pmulher = mean(Pmulher))

item4 <- basededados %>%
  group_by(Ano)  %>%
  mutate(aux1 = sum(PobrezaBrancos),
         aux2 = sum(Brancos),
         aux3 = sum(PobrezaPardos),
         aux4 = sum(Pardos), 
         aux5 = sum(PobrezaPretos),
         aux6 = sum(Pretos),
         Pbrancos = ((aux1/aux2)*100),
         Ppardos = ((aux3/aux4)*100),
         Ppretos = ((aux5/aux6)*100)) %>%
  summarise(Pbrancos = mean(Pbrancos),
            Ppardos = mean(Ppardos),
            Ppretos = mean(Ppretos))

item5 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(PobrezaHomemBranco),
         aux2 = sum(Homensbrancos),
         aux3 = sum(PobrezaHomemPardo),
         aux4 = sum(HomensPardos),
         aux5 = sum(PobrezaHomemPreto),
         aux6 = sum(HomensPretos), 
         aux7 = sum(PobrezaMulherBranca),
         aux8 = sum(MulheresBrancas),
         aux9 = sum(PobrezaMulherParda),
         aux10 = sum(MulheresPardas),
         aux10 = sum(PobrezaMulherPreta),
         aux11 = sum(MulheresPretas),
         PHomembranco = ((aux1/aux2)*100),
         PHomempardo = ((aux3/aux4)*100),
         PHomempreto = ((aux5/aux6)*100),
         PMulherbranca = ((aux7/aux8)*100),
         PMulherparda = ((aux9/aux10)*100),
         PMulhernegra = ((aux11/aux12)*100))
  
item6 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(PobrezaGrupo1),
         aux2 = sum(Grupo1),
         aux3 = sum(PobrezaGrupo2),
         aux4 = sum(Grupo2),
         aux5 = sum(PobrezaGrupo3),
         aux6 = sum(Grupo3),
         aux7 = sum(PobrezaGrupo4),
         aux8 = sum(Grupo4),
         Pgrupo1 = sum((aux1/aux2)*100),
         Pgrupo2 = sum((aux3/aux4)*100),
         Pgrupo3 = sum((aux5/aux6)*100),
         Pgrupo4 = sum((aux7/aux8)*100)) %>%
  summarise(Pgrupo1 = mean(Pgrupo1),
            Pgrupo2 = mean(Pgrupo2),
            Pgrupo3 = mean(Pgrupo3),
            Pgrupo4 = mean(Pgrupo4))


###########################################################
#                    Composição da Renda                  #
###########################################################

item7 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(RendaPobres),
         aux2 = sum(RendaPobresTrabalho),
         aux3 = sum(RendaPobresBPC),
         aux4 = sum(RendaPobresBF),
         aux5 = sum(RendaPobresPSocial),
         aux6 = sum(RendaPobresSegdesemprego),
         aux7 = sum(RendaPobresAposentadoria),
         aux8 = sum(RendaPobresDoacao),
         aux9 = sum(RendaPobresAluguel),
         Ptrabalho = sum((aux2/aux1)*100),
         Pajuda = sum(((aux3+aux4)/aux1)*100),
         Papoaluguel = sum(((aux7+aux9)/aux1)*100),
         Pdoacao = sum((aux8/aux1)*100)) %>%
  summarise(Ptrabalho = mean(Ptrabalho),
            Pajuda = mean(Pajuda),
            Papoaluguel = mean(Papoaluguel),
            Pdoacao = mean(Pdoacao))

Figura7 <- ggplot(item7, aes(x = Ano))+
  geom_bar(aes(y = Ptrabalho, col = "Trbalho")) +
  geom_bar()







item8 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(rendadompc),
         aux2 = sum(RendaTrabalho),
         aux3 = sum(RendaBPC),
         aux4 = sum(RendaBF),
         aux5 = sum(RendaPSocial),
         aux6 = sum(RendaSegdesemprego),
         aux7 = sum(RendaAposentadoria),
         aux8 = sum(RendaDoacao),
         aux9 = sum(RendaAluguel),
         Rtrabalho = sum((aux2/aux1)*100),
         Rajuda = sum(((aux3+aux4)/aux1)*100),
         Rapoaluguel = sum(((aux7+aux9)/aux1)*100),
         Rdoacao = sum((aux8/aux1)*100)) %>%
  summarise(Rtrabalho = mean(Rtrabalho),
            Rajuda = mean(Rajuda),
            Rapoaluguel = mean(Rapoaluguel),
            Rdoacao = mean(Rdoacao))



###########################################################
#                     Hiato da Pobreza                    #
###########################################################

item9 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(HiatoRenda),
         aux2 = sum(HiatoAgregado),
         aux3 = sum(populacao),
         HAgregado = sum((aux2/aux4)*100))
  

