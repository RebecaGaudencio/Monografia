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
                  "2016_visita5",
                  "2017_visita1",
                  "2017_visita5",
                  "2018_visita1",
                  "2018_visita5",
                  "2019_visita1",
                  "2019_visita5")

basededados <- data.frame(UF = character(), Ano = character())
baseproporcao <- data.frame(Ano = character())


#################################################################
##                 Loop p/ ler PNADCs em .csv                  ##
#################################################################

for (xx in lista_visita) {
  setwd(in_dir)
  db1 <- read.csv(paste0(in_dir, "/DadosVisitas", xx, ".csv"))
  basededados <- rbind(basededados, db1)
}

for (xx in lista_visita) {
  setwd(in_dir)
  db2 <- read.csv(paste0(in_dir, "/DadosRenda", xx, ".csv"))
  baseproporcao <- rbind(baseproporcao, db2)
}


###############################################################
#                 Ajuste nas bases de dados                   #
###############################################################

basededados <- basededados %>%
  group_by(UF,Ano) %>%
  summarise(populacao = mean(populacao),
            rendtrabhabit.x = mean(rendtrabhabit.x),
            rendtrabeft.x = mean(rendtrabeft.x),
            rendtrabeft.y = mean(rendtrabeft.y),
            rendpc = mean(rendpc),
            rendpcnordeste = mean(rendpcnordeste),
            rendpcnorte = mean(rendpcnorte),
            rendpcsudeste = mean(rendpcsudeste),
            rendpcsul = mean(rendpcsul),
            rendpcentroeste = mean(rendpcentroeste),
            faixa1rendhab = mean(faixa1rendhab),
            faixa2rendhab = mean(faixa2rendhab),
            faixa3rendhab = mean(faixa3rendhab),
            faixa4rendhab = mean(faixa4rendhab),
            faixa5rendhab = mean(faixa5rendhab),
            faixa6rendhab = mean(faixa6rendhab),
            faixa7rendhab = mean(faixa7rendhab),
            faixa1rendeft = mean(faixa1rendeft),
            faixa2rendeft = mean(faixa2rendeft),
            faixa3rendeft = mean(faixa3rendeft),
            faixa4rendeft = mean(faixa4rendeft),
            faixa5rendeft = mean(faixa5rendeft),
            faixa6rendeft = mean(faixa5rendeft),
            faixa6rendeft = mean(faixa6rendeft),
            faixa7endeft = mean(faixa7endeft),
            BPC = mean(BPC),
            BF = mean(BF),
            PSocial = mean(PSocial),
            Segdesemprego = mean(Segdesemprego),
            Segdesempregonorte = mean(Segdesempregonorte),
            Segdesempregonordeste = mean(Segdesempregonordeste),
            Segdesempregosudeste = mean(Segdesempregosudeste),
            Segdesempregosul = mean(Segdesempregosul),
            Segdesempregocentrooeste = mean(Segdesempregocentrooeste),
            Aposentadoria = mean(Aposentadoria),
            Doacao = mean(Doacao),
            Aluguel = mean(Aluguel),
            GiniEfet = mean(GiniEfet),
            GiniHab = mean(GiniHab))




baseproporcao <- baseproporcao %>%
  group_by(Ano) %>%
  summarise(pop = mean(pop),
            rendapctotal = mean(rendapctotal),
            renda1rico = mean(renda1rico),
            renda5rico = mean(renda5rico),
            renda10rico = mean(renda10rico),
            renda50seguinte = mean(renda50seguinte),
            renda40pobre = mean(renda40pobre),
            renda50pobre = mean(renda50pobre))


####################################################
#       Renda Domiciliar per capita - Brasil       #
####################################################

item1 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(rendpc),
         aux2 = sum(populacao),
         rendapc = (aux1/aux2)) %>%
  summarise(rendapc = mean(rendapc))
         

windowsFonts(Times=windowsFont("Times New Roman"))

Figura1 <- ggplot(data = item1, aes(Ano, rendapc)) +
  geom_line(color = "gray20") + 
  geom_point(shape = 21, color = "black", fill = "indianred1", size = 3) +
  geom_vline(xintercept = item1$Ano[4], linetype = 8) +
  theme_bw() +
  labs(x = "Ano",
       y = "Em %")

setwd(out_dir)

png("Renda_domiciliar_pc.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura1)
dev.off()


#####################################################
#       Renda Domiciliar per capita - Regioes       #
#####################################################

basededados[is.na(basededados)] <- 0

# item2 <- basededados %>%
#  group_by(Ano) %>%
#  mutate(aux1 = sum(rendpcnordeste),
#         aux2 = sum(rendpcnorte),
#         aux3 = sum(rendpcsudeste),
#         aux4 = sum(rendpcsul),
#         aux5 = sum(rendpcentrooeste)
#         aux6 = sum(populacao),
#         rendadompcnordeste = (aux1/aux6),
#         rendadompcnorte = (aux2/aux6),
#         rendadompcsudeste = (aux3/aux6),
#         rendadompcsul = (aux4/aux6),
#         rendadompcentroeste = (aux5/aux6)) %>%
#  summarise(rendadompcnordeste = mean(rendadompcnordeste),
#            rendadompcnorte = mean(rendadompcnorte),
#            rendadompcsudeste = mean(rendadompcsudeste),
#            rendadompcsul = mean(rendadompcsul),
#            rendadompcentroeste = mean(rendadompcentroeste))



Figura2 <- ggplot(item2, aes(x = Ano)) + 
  geom_line(aes(y = rendadompcnordeste, col = "Nordeste"), size = 1.2) +
  geom_line(aes(y = rendadompcnorte, col = "Norte"), size = 1.2) +
  geom_line(aes(y = rendadompcsudeste, col = "Sudeste"), size = 1.2) +
  geom_line(aes(y = rendadompcsul, col = "Sul"), size = 1.2) +
  geom_line(aes(y = rendadompcentrooeste, col = "Centro Oeste"), size = 1.2) +
  geom_vline(xintercept = item1$Tempo[32], linetype = 8) +
  theme_bw() +
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c("bisque3", "black", "gray69", "tan4", "forestgreen")) +
  labs(x = "Ano",
       y = "Em %",
       color = "") +
  theme(legend.position = 'bottom')

# Salvando a imagem #
setwd(out_dir)

png("Renda_domiciliar_pc_Regiao.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura2)
dev.off()


####################################################
#                   Beneficios                     #
####################################################


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



#################################################
#      % Renda apropriada por percentis         #
#################################################

item6 <- baseproporcao %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(renda1rico),
         aux2 = sum(renda5rico),
         aux3 = sum(renda10rico),
         aux4 = sum(renda50seguinte),
         aux5 = sum(renda40pobre),
         aux6 = sum(renda50pobre),
         aux7 = sum(rendapctotal),
         rico1 = (aux1/aux7)*100,
         rico5 = (aux2/aux7)*100,
         rico10 = (aux3/aux7)*100,
         seguinte50 = (aux4/aux7)*100,
         pobre40 = (aux5/aux7)*100,
         pobre50 = (aux6/aux7)*100) %>%
  summarise(rico1 = mean(rico1),
            rico5 = mean(rico5),
            rico10 = mean(rico10), 
            seguinte50 = mean(seguinte50),
            pobre40 = mean(pobre40),
            pobre50 = mean(pobre50))

Figura6 <- ggplot(item6, aes(x = Ano)) +
  geom_bar(aes(y = rico1, color = "1% mais rico"), stat = "identity", width = .15, fill = "darksalmon" , position = position_nudge(x = .0)) +
  geom_bar(aes(y = rico5, color = "5% mais ricos"), stat = "identity", width = .15, fill = "black", position = position_nudge(x = .15)) +
  geom_bar(aes(y = rico10, color = "10% mais ricos"), stat = "identity", width = .15, fill = "orangered4" ,position = position_nudge(x = .3) ) +
  geom_bar(aes(y = seguinte50, color = "50% seguintes" ), stat = "identity", width = .15, fill = "tan4", position = position_nudge(x = .45)) +
  theme_bw() +
  scale_color_manual(values = c("darksalmon", "black", "tan4", "orangered4")) +
  scale_fill_manual(values = c("rico1" = "darksalmon", "black", "orangered4","50% seguintes")) + 
  labs(x = "Ano",
       y = "Em %",
       color = "") +
  theme(legend.position = 'bottom')

plot(Figura6)


#         
#(baseproporcao$renda1rico[1]/baseproporcao$rendapctotal[1])*100



