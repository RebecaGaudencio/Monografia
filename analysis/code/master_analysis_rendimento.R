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
library(wesanderson)

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
                  "2018_visita5",
                  "2019_visita1"
                  )

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

#####################################################
#    Renda Domiciliar per capita - BR e Regioes     #
#####################################################

basededados[is.na(basededados)] <- 0

item1 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(rendpcnordeste),
         aux2 = sum(popNordeste),
         aux3 = sum(rendpcnorte),
         aux4 = sum(popNorte),
         aux5 = sum(rendpcsudeste),
         aux6 = sum(popSudeste),
         aux7 = sum(rendpcsul),
         aux8 = sum(popSul),
         aux9 = sum(rendpcentroeste),
         aux10 = sum(popCentroOeste),
         aux11 = sum(rendpc),
         aux12 = sum(populacao),
         rendapc = (aux11/aux12),
         rendadompcnordeste = (aux1/aux2),
         rendadompcnorte = (aux3/aux4),
         rendadompcsudeste = (aux5/aux6),
         rendadompcsul = (aux7/aux8),
         rendadompcentroeste = (aux9/aux10)) %>%
  summarise(rendapc = mean(rendapc),
            rendadompcnordeste = mean(rendadompcnordeste),
            rendadompcnorte = mean(rendadompcnorte),
            rendadompcsudeste = mean(rendadompcsudeste),
            rendadompcsul = mean(rendadompcsul),
            rendadompcentroeste = mean(rendadompcentroeste))


Figura1 <- ggplot(item1, aes(x = Ano)) + 
  geom_line(aes(y = rendapc, col = "Brasil"), size = 1.0) +
  geom_line(aes(y = rendadompcnordeste, col = "Nordeste"), size = 1.0) +
  geom_line(aes(y = rendadompcnorte, col = "Norte"), size = 1.0) +
  geom_line(aes(y = rendadompcsudeste, col = "Sudeste"), size = 1.0) +
  geom_line(aes(y = rendadompcsul, col = "Sul"), size = 1.0) +
  geom_line(aes(y = rendadompcentroeste, col = "Centro Oeste"), size = 1.0) +
  geom_point(aes(y = rendapc , col = "Brasil"), color = "black", shape = 16, size = 2.5 ) +
  geom_point(aes(y = rendadompcnordeste , col = "Nordeste"), color = "burlywood4", shape = 16, size = 2.5 ) +
  geom_point(aes(y = rendadompcnorte , col = "Norte"), color = "burlywood", shape = 16 , size = 2.5) +
  geom_point(aes(y = rendadompcsudeste, col = "Sudeste"), color = "bisque3", shape = 16, size = 2.5) +
  geom_point(aes(y = rendadompcsul, col = "Sul"), color = "tan4", shape = 16, size = 2.5) +
  geom_point(aes(y = rendadompcentroeste, col = "Centro Oeste"), color = "tomato3", shape = 16, size = 2.5) +
  theme_bw() +
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c("black", "tomato3", "burlywood4", "burlywood", "bisque3", "tan4")) +
  labs(x = "Ano",
       y = "Em R$",
       color = "") +
  theme(legend.position = 'bottom')

# Salvando a imagem #
setwd(out_dir)

png("Renda_domiciliar_pc_BR_Regio.png", units = "px", width = 850, height = 536, res = 110)
plot(Figura1)
dev.off()


#################################################
#                Faixas de Renda                #
#################################################

item2 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(faixa1rendhab),
         aux2 = sum(faixa2rendhab),
         aux3 = sum(faixa3rendhab),
         aux4 = sum(faixa4rendhab),
         aux5 = sum(faixa5rendhab),
         aux6 = sum(faixa6rendhab),
         aux7 = sum(faixa7rendhab),
         aux8 = sum(populacao),
         faixa1 = (aux1/aux8)*100,
         faixa2 = (aux2/aux8)*100,
         faixa3 = (aux3/aux8)*100,
         faixa4 = (aux4/aux8)*100,
         faixa5 = (aux5/aux8)*100,
         faixa6 = (aux6/aux8)*100,
         faixa7 = (aux7/aux8)*100) %>%
  summarise(faixa1 = mean(faixa1),
            faixa2 = mean(faixa2),
            faixa3 = mean(faixa3), 
            faixa4 = mean(faixa4),
            faixa5 = mean(faixa5),
            faixa6 = mean(faixa6),
            faixa7 = mean(faixa7))


Figura2 <- ggplot(item2, aes(x = Ano)) +
  geom_line(aes(y = faixa1, col = "Até 1/4 do SM"), size = 1.0) +
  geom_line(aes(y = faixa2, col = "Mais de 1/4 até 1/2 SM"), size = 1.0) +
  geom_line(aes(y = faixa3, col = "Mais de 1/2 até 1 SM"), size = 1.0) +
  geom_line(aes(y = faixa4, col = "Mais de 1 até 2 SM"), size = 1.0) +
  geom_line(aes(y = faixa5, col = "Mais de 2 até 3 SM"), size = 1.0) +
  geom_line(aes(y = faixa6, col = "Mais de 3 até 5 SM"), size = 1.0) +
  geom_line(aes(y = faixa7, col = "Mais de 5 SM"), size = 1.0) +
  geom_point(aes(y = faixa1, col = "Até 1/4 do SM"), color = "sandybrown", shape = 16, size = 2.5 ) +
  geom_point(aes(y = faixa2, col = "Mais de 1/4 até 1/2 SM"), color = "khaki", shape = 16, size = 2.5 ) +
  geom_point(aes(y = faixa3, col = "Mais de 1/2 até 1 SM"), color = "darkgreen", shape = 16 , size = 2.5) +
  geom_point(aes(y = faixa4, col = "Mais de 1 até 2 SM"), color = "lightgreen", shape = 16, size = 2.5) +
  geom_point(aes(y = faixa5, col = "Mais de 2 até 3 SM"), color = "orangered2", shape = 16, size = 2.5) +
  geom_point(aes(y = faixa6, col = "Mais de 3 até 5 SM"), color = "saddlebrown", shape = 16, size = 2.5) +
  geom_point(aes(y = faixa7, col = "Mais de 5 SM"), color = "black", shape = 16, size = 2.5) +
  theme_bw() +
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c("sandybrown", "lightgreen", "darkgreen", "khaki", "orangered", "saddlebrown", "black")) +
  labs(x = "Ano",
       y = "Em %",
       color = "") +
  theme(legend.position = 'bottom')

setwd(out_dir)

png("Faixas_Salariais", units = "px", width = 850, height = 536, res = 110)
plot(Figura2)
dev.off()

#################################################
#      % Renda apropriada por percentis         #
#################################################

item3 <- baseproporcao %>%
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

item3_aux <- item5 %>%
  pivot_longer(cols = c(rico1, rico5, rico10, pobre40, pobre50),
               names_to = "Percentil")


Figura3 <- ggplot(item5, aes(x = Ano)) +
  geom_bar(aes(y = rico1, col = "1% mais rico"), 
           fill = "darksalmon", stat = "identity", width = .15, position = position_nudge(x = .0)) +
  geom_bar(aes(y = rico5, col = "5% mais ricos"), 
           fill = "black", stat = "identity", width = .15, position = position_nudge(x = .15)) +
  geom_bar(aes(y = rico10, col = "10% mais ricos"), 
           fill = "orangered4", stat = "identity", width = .15, position = position_nudge(x = .3)) +
  geom_bar(aes(y = pobre50, col = "40% mais pobres"), 
           fill = "tan4", stat = "identity", width = .15, position = position_nudge(x = .45)) +
  geom_bar(aes(y = pobre40, col = "50% mais pobres"),
           fill = "goldenrod3", stat = "identity", width = .15, position = position_nudge(x = .60)) +
  theme_bw() +
  labs(x = "Ano",
       y = "Em %",
       color = "") +
  theme(legend.position = 'bottom')

plot(Figura3)


Figura4 <- ggplot(item5, aes(x = Ano)) +
  geom_bar(aes(y = rico1, color = "1% mais rico"), 
           fill = "darksalmon", stat = "identity", width = .15, position = position_nudge(x = .0)) +
  geom_bar(aes(y = rico5, color = "5% mais ricos"), 
           fill = "black", stat = "identity", width = .15, position = position_nudge(x = .15)) +
  geom_bar(aes(y = rico10, color = "10% mais ricos"), 
           fill = "orangered4", stat = "identity", width = .15, position = position_nudge(x = .3)) +
  geom_bar(aes(y = pobre50, color = "50% mais pobres" ), 
           fill = "tan4", stat = "identity", width = .15, position = position_nudge(x = .45)) +
  geom_bar(aes(y = pobre40, color = "40% mais pobres" ),
           fill = "goldenrod3", stat = "identity", width = .15, position = position_nudge(x = .60)) +
  theme_bw() +
  scale_linetype_manual(values = c("darksalmon", "black", "orangered4", "goldenrod3", "tan4")) +
  labs(x = "Ano",
       y = "Em %",
       color = "") +
  theme(legend.position = 'bottom')

plot(Figura4)

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
#                 Indice de Gini                #
#################################################

item8 <- baseproporcao %>%
  group_by(Ano) %>%
  summarise(Gini = mean(GiniBR))


  





