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
library(formattable)

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

year = zoo::as.Date(as.yearmon(baseproporcao$Ano))
baseproporcao = baseproporcao %>%
  mutate(Year = zoo::as.Date(as.yearmon(Ano)))

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
  geom_line(aes(y = rendadompcnordeste, col = "Nordeste"), size = 1.05) +
  geom_line(aes(y = rendadompcnorte, col = "Norte"), size = 1.05) +
  geom_line(aes(y = rendapc, col = "Brasil"), size = 1.05) +
  geom_line(aes(y = rendadompcsudeste, col = "Sudeste"), size = 1.05) +
  geom_line(aes(y = rendadompcsul, col = "Sul"), size = 1.05) +
  geom_line(aes(y = rendadompcentroeste, col = "Centro Oeste"), size = 1.05) +
  theme_bw() +
  scale_color_manual(values = c("#56B4E9", "mediumblue", "tan2", "gray55", "gray76", "black"),
                     breaks = c("Norte", "Nordeste", "Brasil", "Centro Oeste", "Sul", "Sudeste")) +
  labs(x = "Ano",
       y = "Em R$",
       color = "") +
  ylab("Em R$\n") +
  theme(legend.position = 'bottom')

plot(Figura1)

 teste <- ggplot(item1, aes(x = Ano)) +
  geom_bar(aes(y = rendadompcnordeste, fill = "Nordeste"), stat = "identity") +
  geom_bar(aes(y = rendadompcnorte, fill = "Norte"), stat = "identity") +
  geom_bar(aes(y = rendapc, fill = "Brasil"), stat = "identity") +
  geom_bar(aes(y = rendadompcsudeste, fill = "Sudeste"), stat = "identity") +
  geom_bar(aes(y = rendadompcsul, fill = "Sul"), stat = "identity") +
  geom_bar(aes(y = rendadompcentroeste, fill = "Centro Oeste"), stat = "identity") +
  theme_bw() +
  scale_fill_manual(values = c("#56B4E9", "mediumblue", "tan2", "gray55", "gray76", "black"),
                     breaks = c("Norte", "Nordeste", "Brasil", "Centro Oeste", "Sul", "Sudeste")) +
  guides(fill = guide_legend(title = " ")) +
  labs(x = "Ano",
       y = "Em R$",
       color = "") +
  ylab("Em R$\n") +
  theme(legend.position = 'bottom')

teste + coord_flip()


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
  geom_line(aes(y = faixa1, col = "Até 1/4 do SM"), size = 1.2) +
  geom_line(aes(y = faixa2, col = "Mais de 1/4 até 1/2 SM"), size = 1.2) +
  geom_line(aes(y = faixa3, col = "Mais de 1/2 até 1 SM"), size = 1.2) +
  geom_line(aes(y = faixa4, col = "Mais de 1 até 2 SM"), size = 1.2) +
  geom_line(aes(y = faixa5, col = "Mais de 2 até 3 SM"), size = 1.2) +
  geom_line(aes(y = faixa6, col = "Mais de 3 até 5 SM"), size = 1.2) +
  geom_line(aes(y = faixa7, col = "Mais de 5 SM"), size = 1.2) +
  geom_point(aes(y = faixa1, color = "Até 1/4 do SM"),
             color = "#98AFC7",  shape = 21, fill = "#98AFC7", size = 3) +
  geom_point(aes(y = faixa2, color = "Mais de 1/4 até 1/2 SM"),
             color = "#C0C0C0",  shape = 21, fill = "#C0C0C0", size = 3) +
  geom_point(aes(y = faixa3, color = "Mais de 1/2 até 1 SM"),
             color = "#000000",  shape = 21, fill = "#000000", size = 3) +
  geom_point(aes(y = faixa4, color = "Mais de 1 até 2 SM"),
             color = "#736F6E",  shape = 21, fill = "#736F6E", size = 3) +
  geom_point(aes(y = faixa5, color = "Mais de 2 até 3 SM"),
             color = "#90CAF9",  shape = 21, fill = "#90CAF9", size = 3) +
  geom_point(aes(y = faixa6, color = "Mais de 3 até 5 SM"),
             color = "#6698FF",  shape = 21, fill = "#6698FF", size = 3) +
  geom_point(aes(y = faixa7, color = "Mais de 5 SM"),
             color = "#0D47A1",  shape = 21, fill = "#0D47A1", size = 3) +
  theme_bw() +
  scale_color_manual("Faixas Salariais",
                     values = c("#000000", "#736F6E", "#C0C0C0", "#98AFC7", 
                                "#90CAF9", "#6698FF",  "#0D47A1"),
                     breaks = c("Mais de 1/2 até 1 SM",
                                "Mais de 1 até 2 SM",
                                "Mais de 1/4 até 1/2 SM",
                                "Até 1/4 do SM",
                                "Mais de 2 até 3 SM",
                                "Mais de 3 até 5 SM",
                                "Mais de 5 SM"
                                )) +
  guides(fill = guide_legend(title = "Faixa de Renda")) +
  labs(x = "Ano",
       y = "População (%)",
       color = "") 

plot(Figura2)

setwd(out_dir)
png("Faixas_Salariais.png", units = "px", width = 850, height = 536, res = 110)
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
         aux5 = sum(renda40pobre),
         aux6 = sum(renda50pobre),
         aux7 = sum(rendapctotal),
         rico1 = (aux1/aux7)*100,
         rico5 = (aux2/aux7)*100,
         rico10 = (aux3/aux7)*100,
         pobre40 = (aux5/aux7)*100,
         pobre50 = (aux6/aux7)*100) %>%
  summarise(rico1 = mean(rico1),
            rico5 = mean(rico5),
            rico10 = mean(rico10), 
            pobre40 = mean(pobre40),
            pobre50 = mean(pobre50))

Figura3 <- ggplot(item3, aes(x = Ano)) +
  geom_bar(aes(y = pobre40, fill = "40% mais pobres"),
           stat = "identity", width = .15, position = position_nudge(x = -0.25)) +
  geom_bar(aes(y = pobre50, fill = "50% mais pobres"), 
           stat = "identity", width = .15, position = position_nudge(x = -0.1)) +
  geom_bar(aes(y = rico10, fill = "10% mais ricos"), 
           stat = "identity", width = .15, position = position_nudge(x = 0.05)) +
  geom_bar(aes(y = rico5, fill = "5% mais ricos"), 
           stat = "identity", width = .15, position = position_nudge(x = 0.2)) +
  geom_bar(aes(y = rico1, fill = "1% mais rico"),
           stat = "identity", width = .15, position = position_nudge(x = 0.35)) +
      theme_bw() +
  scale_fill_manual(values = c("#98AFC7", "#6698FF", "#C0C0C0", "#736F6E", "#000000"),
                    breaks = c("40% mais pobres",
                               "50% mais pobres",
                               "10% mais ricos",
                               "5% mais ricos",
                               "1% mais rico"
                    )) +
  guides(fill = guide_legend(title = "Percentis")) +
  labs(x = "Ano",
       y = "Em %",
       color = "") +
  theme(legend.position = 'bottom')

plot(Figura3)


# Salvando a imagem #

setwd(out_dir)
png("% da Renda apropriada por percentis.png", units = "px", width = 850, height = 536, res = 115)
plot(Figura3)
dev.off()

########################################################
#      Rendimento do Trabalho - habitual e efetivo     #
########################################################

item4 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(rendtrabhabit.x),
         aux2 = sum(rendtrabeft.x),
         aux3 = sum(populacao),
         trabhabitpc = (aux1/aux3),
         trabefetpc = (aux2/aux3),
         trabhab = aux1,
         trabefe = aux2) %>%
  summarise(trabhabitpc = mean(trabhabitpc),
            trabefetpc = mean(trabefetpc),
            vartrabpc = mean(((trabefetpc-trabhabitpc)/trabhabitpc)*100), 
            trabhab = mean(trabhab), 
            trabefe = mean(trabefe))


Figura5 <- ggplot(item4, aes(x = Ano)) +
  geom_line(aes(y = trabhabitpc, col = "Habitual"), size = 1.0) +
  geom_line(aes(y = trabefetpc, col = "Efetivo"), size = 1.0) +
  geom_point(aes(y = trabhabitpc, col = "Habitual"), color = "tomato3", shape = 16, size = 2.5 ) +
  geom_point(aes(y = trabefetpc, col = "Efetivo"), color = "black", shape = 16, size = 2.5 ) +
  theme_bw() +
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c("black", "tomato3")) +
  labs(x = "Ano",
       y = "Em R$",
       color = "") +
  theme(legend.position = 'bottom')
  
plot(Figura5)

setwd(out_dir)
png("Rendimento_Trabalho.png", units = "px", width = 850, height = 536, res = 110)
plot(Figura5)
dev.off()

Figura6 <- ggplot(item4, aes(x = Ano)) +
  geom_line(aes(y = vartrabpc, col = "Variação %"), size = 1.0) +
  labs(x = "Ano",
       y = "Em %",
       color = "") +
  theme(legend.position = 'bottom')
  
plot(Figura6)


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

# Gini EFETIVO

item7 <- baseproporcao %>%
  group_by(Ano) %>%
  summarise(GiniEf = mean(GiniEf))

item7$GiniEf <- formattable(item7$GiniEf, digits = 3, format = "f")

windowsFonts(Times=windowsFont("Times New Roman"))

Figura7 <- ggplot(item7, aes(Ano, GiniEf)) +
  geom_text(aes(label = GiniEf), vjust = 2) +
  geom_line(color ="gray20") +
  geom_point(shape = 21, color = "black", fill = "indianred1", size = 3) +
  theme_bw() +
  coord_cartesian(ylim = c(0.5, 0.54)) +
  labs(x = "Ano",
       y = "Coeficiente de Gini") +
  ylab("Coeficiente de Gini\n") +
  theme(legend.position = 'bottom') 

plot(Figura7)
  
setwd(out_dir)
png("Coeficiente_Gini.png", units = "px", width = 850, height = 536, res = 110)
plot(Figura7)
dev.off()


# Gini HABITUAL

item8 <- baseproporcao %>%
  group_by(Ano) %>%
  summarise(GiniHa = mean(GiniHa))

item8$GiniHa <- formattable(item8$GiniHa, digits = 3, format = "f")

windowsFonts(Times=windowsFont("Times New Roman"))

Figura8 <- ggplot(item8, aes(Ano, GiniHa)) +
  geom_text(aes(label = GiniHa), vjust = 2) +
  geom_line(color ="gray20") +
  geom_point(shape = 21, color = "black", fill = "indianred1", size = 3) +
  theme_bw() +
  coord_cartesian(ylim = c(0.45, 0.55)) +
  labs(x = "Ano",
       y = "Coeficiente de Gini") +
  ylab("Coeficiente de Gini\n") +
  theme(legend.position = 'bottom') 

plot(Figura8)

setwd(out_dir)
png("Coeficiente_Gini_Habitual.png", units = "px", width = 850, height = 536, res = 110)
plot(Figura8)
dev.off()



##################################################
#     Desocupação por Percentis de Renda         #
##################################################


item9 <- baseproporcao %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(desitem1),
         aux2 = sum(desitem2),
         aux3 = sum(desitem3),
         aux5 = sum(desitem5),
         aux6 = sum(desitem6),
         aux7 = sum(pop*0.01),
         aux8 = sum(pop*0.05),
         aux9 = sum(pop*0.1),
         aux10 = sum(pop*0.4),
         aux11 = sum(pop*0.5),
         desocup1 = (aux1/aux7)*100,
         desocup2 = (aux2/aux8)*100,
         desocup3 = (aux3/aux9)*100,
         desocup4 = (aux5/aux10)*100,
         desocup5 = (aux6/aux11)*100) %>%
  summarise(desocup1 = mean(desocup1),
            desocup2 = mean(desocup2),
            desocup3 = mean(desocup3),
            desocup4 = mean(desocup4),
            desocup5 = mean(desocup5))


Figura9 <- ggplot(item9, aes(x = Ano)) +
  geom_bar(aes(y = desocup1, fill = "1% mais rico"),
           stat = "identity", width = .15, position = position_nudge(x = -0.25)) +
  geom_bar(aes(y = desocup2, fill = "5% mais ricos"), 
           stat = "identity", width = .15, position = position_nudge(x = -0.1)) +
  geom_bar(aes(y = desocup3, fill = "10% mais ricos"), 
           stat = "identity", width = .15, position = position_nudge(x = 0.05)) +
  geom_bar(aes(y = desocup4, fill = "50% mais pobres"), 
           stat = "identity", width = .15, position = position_nudge(x = 0.2)) +
  geom_bar(aes(y = desocup5, fill = "40% mais pobres"),
           stat = "identity", width = .15, position = position_nudge(x = 0.35)) +
  theme_bw() +
  scale_fill_manual(values = c("#000000", "#736F6E", "#C0C0C0", "#98AFC7", "#6698FF"),
                    breaks = c("1% mais rico",
                               "5% mais ricos",
                               "10% mais ricos",
                               "50% mais pobres",
                               "40% mais pobres"
                    )) +
  guides(fill = guide_legend(title = "Percentis")) +
  labs(x = "Ano",
       y = "Em %",
       color = "") +
  theme(legend.position = 'bottom')

plot(Figura9)

setwd(out_dir)
png("Desocupacao_Perecentis_Renda.png", units = "px", width = 850, height = 536, res = 110)
plot(Figura9)
dev.off()
