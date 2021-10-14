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
               "PNADC_012021",
               "PNADC_022021"
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

windowsFonts(Times=windowsFont("Times New Roman"))

Figura1 <- ggplot(data = item1, aes(Tempo, taxadesemprego)) +
  geom_line(color = "gray20") + 
  geom_point(shape = 21, color = "black", fill = "indianred1", size = 3) +
  geom_vline(xintercept = item1$Tempo[33], linetype = 8) +
  theme_bw() +
  labs(x = "Trimestre",
       y = "Em %") +
  xlab("\nTrimestre")
  ylab("Em %\n")+
  theme(plot.title = element_text(family = "Times"))

plot(Figura1)

setwd(out_dir)
png("Evolucao_do_Desemprego.png", units = "px", width = 850, height = 536, res = 110)
plot(Figura1)
dev.off()

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

windowsFonts(Times = windowsFont("Times New Roman"))

Figura2 <- ggplot(item1, aes(x = Tempo, y = taxadedesempregobrancos)) +
  geom_line(aes(col = "Brancos"), size = 1.1) +
  geom_line(aes(y = taxadedesempregonegros , col = "Negros"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoamarelos, col = "Amarelos"),size = 1.1) +
  geom_line(aes(y = taxadedesempregopardos, col = "Pardos"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoindios, col = "Indígenas"), size = 1.1) +
  geom_vline(xintercept = item1$Tempo[33], linetype = 8) +
  theme_bw() +
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c("#000000", "#736F6E", "#C0C0C0", "#98AFC7", "#6698FF"),
                     breaks = c("Negros",
                                "Pardos",
                                "Indígenas",
                                "Brancos",
                                "Amarelos")) +
  labs(x = "Trimestre",
       y = "Em %",
       color = "") +
  theme(legend.position = 'bottom') +
  theme(plot.title = element_text(family = "Times"))

plot(Figura2)

setwd(out_dir)
png("Evolucao_do_Desemprego_por_Raca.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura2)
dev.off()

############################################################
#         Taxa de Desemprego por Escolaridade              #
############################################################

basededados[is.na(basededados)] <- 0 

item3 <- basededados %>%
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

Figura3 <- ggplot(item3, aes(x = Tempo)) +
  geom_line(aes(y = taxadedesempregoesco1, col = "Sem Instrução"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoesco2, col = "Fundamental Incompleto"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoesco3, col = "Fundamental Completo"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoesco4, col = "Médio Incompleto"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoesco5, col = "Médio Completo"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoesco6, col = "Superior Incompleto"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoesco7, col = "Superior Completo"), size = 1.1) +
    geom_vline(xintercept = item3$Tempo[33], linetype = 8) +
  theme_bw() +
  scale_color_manual(values = c("#000000", "#736F6E", "#C0C0C0",  
                                "palegreen3", "#98AFC7", "#6698FF", "dodgerblue4"),
                     breaks = c("Médio Incompleto",
                                "Superior Incompleto",
                                "Médio Completo",
                                "Fundamental Completo",
                                "Fundamental Incompleto",
                                "Sem Instrução",
                                "Superior Completo")) +
  labs(x = "Trimestre",
       y = "Em %",
       color = "") +
  ylab("Em %\n")
  theme(legend.position = 'bottom')+
  theme(plot.title = element_text(family = "Times"))

plot(Figura3)


setwd(out_dir)
png("Evolucao_do_Desemprego_por_Escolaridade.png", units = "px", width = 850, height = 536, res = 110)
plot(Figura3)
dev.off()

############################################################
#          Taxa de Desemprego por Faixa Etaria             #
############################################################

basededados[is.na(basededados)] <- 0 

item4 <- basededados %>%
  group_by(Tempo) %>%
  mutate(aux1 = sum(desocup1), aux2 = sum(ocup1), 
         aux3 = sum(desocup2), aux4 = sum(ocup2),
         aux5 = sum(desocup3), aux6 = sum(ocup3),
         aux7 = sum(desocup4), aux8 = sum(ocup4),
         taxadedesempregoadol   = (aux1/(aux1+aux2))*100,
         taxadedesempregojovens = (aux3/(aux3+aux4))*100,
         taxadedesempregoadultos = (aux5/(aux5+aux6))*100,
         taxadedesempregoidosos = (aux7/(aux7+aux8))*100) %>%
  summarise(taxadedesempregoadol = mean(taxadedesempregoadol),
            taxadedesempregojovens = mean(taxadedesempregojovens),
            taxadedesempregoadultos = mean(taxadedesempregoadultos),
            taxadedesempregoidosos = mean(taxadedesempregoidosos))

windowsFonts(Times=windowsFont("Times New Roman"))

Figura4 <- ggplot(item4, aes(x = Tempo, y = taxadedesempregoadol)) +
  geom_line(aes(col = "Adolescentes"), size = 1.1) +
  geom_line(aes(y = taxadedesempregojovens, col = "Jovens"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoadultos, col = "Adultos"), size = 1.1) +
  geom_line(aes(y = taxadedesempregoidosos, col = "Idosos"), size = 1.1) + 
  geom_vline(xintercept = item4$Tempo[33], linetype = 8) +
  theme_bw() +
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c("#000000", "#736F6E", "#98AFC7", "#6698FF"),
                     breaks = c("Adolescentes",
                                "Jovens",
                                "Adultos",
                                "Idosos")) +
  labs(x = "Trimestre",
       y = "Em %",
       color = "") +
  ylab("Em % \n") +
  theme(legend.position = 'bottom') +
  theme(plot.title = element_text(family = "Times"))

plot(Figura4)


setwd(out_dir)
png("Evolucao_do_Desemprego_por_Faixa_Etaria.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura4)
dev.off()

############################################################
#             Taxa de Desemprego por Região                #
############################################################

basededados[is.na(basededados)] <- 0

item5 <- basededados %>%
  group_by(Tempo) %>%
  mutate(aux1 = sum(desocupnorte), aux2 = sum(ocupnorte), 
         aux3 = sum(desocupnordeste), aux4 = sum(ocupnordeste),
         aux5 = sum(desocupsudeste), aux6 = sum(ocupsudeste),
         aux7 = sum(desocupsul), aux8 = sum(ocupsul),
         aux9 = sum(desocupcentrooeste), aux10 = sum(ocupcentrooeste),
         taxadedesempregonorte   = (aux1/(aux1+aux2))*100,
         taxadedesempregonordeste = (aux3/(aux3+aux4))*100,
         taxadedesempregosudeste = (aux5/(aux5+aux6))*100,
         taxadedesempregosul = (aux7/(aux7+aux8))*100,
         taxadedesempregocentrooeste = (aux9/(aux9+aux10))*100) %>%
  summarise(taxadedesempregonorte = mean(taxadedesempregonorte),
            taxadedesempregonordeste = mean(taxadedesempregonordeste),
            taxadedesempregosudeste = mean(taxadedesempregosudeste),
            taxadedesempregosul = mean(taxadedesempregosul),
            taxadedesempregocentrooeste = mean(taxadedesempregocentrooeste))

windowsFonts(Times=windowsFont("Times New Roman"))

Figura5 <- ggplot(item5, aes(x = Tempo, y = taxadedesempregonorte)) +
  geom_line(aes(col = "Norte"), size = 1.2) +
  geom_line(aes(y = taxadedesempregonordeste, col = "Nordeste"), size = 1.2) +
  geom_line(aes(y = taxadedesempregosudeste, col = "Sudeste"), size = 1.2) +
  geom_line(aes(y = taxadedesempregosul, col = "Sul"), size = 1.2) +
  geom_line(aes(y = taxadedesempregocentrooeste, col = "Centro Oeste"), size = 1.2) +
  geom_vline(xintercept = item1$Tempo[33], linetype = 8) +
  theme_bw() +
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c("#000000", "#736F6E", "#C0C0C0", "#98AFC7", "#6698FF"),
                     breaks = c("Nordeste",
                                "Sudeste",
                                "Norte",
                                "Centro Oeste",
                                "Sul")) +
  labs(x = "Trimestre",
       y = "Em %",
       color = "") +
  theme(legend.position = 'bottom') +
  theme(plot.title = element_text(family = "Times"))

plot(Figura5)

# Salvando a imagem #

setwd(out_dir)
png("Evolucao_do_Desemprego_por_Regiao.png", units = "px", width = 850, height = 536, res = 110)
plot(Figura5)
dev.off()

############################################################
#             Taxa de Desemprego por Genero                #
############################################################

basededados[is.na(basededados)] <- 0

item1 <- basededados %>%
  group_by(Tempo) %>%
  mutate(aux1 = sum(desocupfem), aux2 = sum(ocupfem), 
         aux3 = sum(desocupmasc), aux4 = sum(ocupmasc),
         taxadedesempregofem = (aux1/(aux1+aux2))*100,
         taxadedesempregomasc = (aux3/(aux3+aux4))*100) %>%
  summarise(taxadedesempregofem = mean(taxadedesempregofem),
            taxadedesempregomasc = mean(taxadedesempregomasc))

windowsFonts(Times=windowsFont("Times New Roman"))

Figura6 <- ggplot(item1, aes(x = Tempo, y = taxadedesempregofem)) +
  geom_line(aes(col = "Feminino"), size = 1.2) +
  geom_line(aes(y = taxadedesempregomasc, col = "Masculino"), size = 1.2) +
  geom_vline(xintercept = item1$Tempo[32], linetype = 8) +
  theme_bw() +
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c("bisque3", "black")) +
  labs(x = "Trimestre",
       y = "Em %",
       title = "Evolução do Desemprego por Gênero",
       color = "") +
  theme(legend.position = 'bottom') +
  theme(plot.title = element_text(family = "Times"))


# Salvando a imagem #
setwd(out_dir)

png("Evolucao_do_Desemprego_por_Genero.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura6)
dev.off()

#######################################################
#               Compilado de Desemprego               #
#######################################################

item1 <- basededados %>%
  group_by(Tempo) %>%
  mutate(aux1 = sum(desocupnordeste), aux2 = sum(ocupnordeste),
         aux3 = sum(desocupesco4), aux4 = sum(ocupesco4),
         aux5 = sum(desocupcor2), aux6 = sum(ocupcor2),
         aux7 = sum(desocup1), aux8 = sum(ocup1),
         taxadedesempregonordeste = (aux1/(aux1+aux2))*100,
         taxadedesempregoesco4 = (aux3/(aux3+aux4))*100,
         taxadedesempregonegros = (aux5/(aux5+aux6))*100,
         taxadedesempregoadol   = (aux7/(aux7+aux8))*100) %>%
  summarise(taxadedesempregonordeste = mean(taxadedesempregonordeste),
            taxadedesempregoesco4 = mean(taxadedesempregoesco4),
            taxadedesempregonegros = mean(taxadedesempregonegros),
            taxadedesempregoadol = mean(taxadedesempregoadol))


windowsFonts(Times=windowsFont("Times New Roman"))

Figura7 <- ggplot(item1, aes(x = Tempo)) +
  geom_line(aes(y = taxadedesempregonordeste, col = "Nordeste"), size = 1.2) +
  geom_line(aes(y = taxadedesempregoesco4 , col = "Médio Incompleto"), size = 1.2) +
  geom_line(aes(y = taxadedesempregonegros, col = "Negros"), size = 1.2) +
  geom_line(aes(y = taxadedesempregoadol, col = "Adolescentes"), size = 1.2) +
  geom_point(aes(y = taxadedesempregonordeste, col = "Nordeste"), color = "khaki4", shape = 17, size = 2.5 ) +
  geom_point(aes(y = taxadedesempregoesco4 , col = "Médio Incompleto"), color = "tan4", shape = 17 , size = 2.5) +
  geom_point(aes(y = taxadedesempregonegros, col = "Negros"), color = "black", shape = 17, size = 2.5) +
  geom_point(aes(y = taxadedesempregoadol, col = "Adolescentes"), color = "darkolivegreen", shape = 17, size = 2.5) +
  geom_vline(xintercept = item1$Tempo[32], linetype = 8, color ="gray49", size = 1) +
  theme_bw() +
  scale_linetype_manual(values = c("twodash", "dotted")) +
  scale_color_manual(values = c("darkolivegreen", "tan4", "black", "khaki4")) +
  labs(x = "Trimestre",
       y = "Em %",
       title = "Evolução do Desemprego" ,
       subtitle = "Nordeste, Pessoas com Ensino Médio Incompleto, Negros e Adolescentes - Brasil (2012.1 - 2021.1)",
       color = "") +
  theme(legend.position = 'bottom') +
  theme(plot.title = element_text(family = "Times"),
        plot.subtitle = element_text(family = "Times"))

setwd(out_dir)

png("Evolucao_do_Desemprego_Compilaado.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura7)
dev.off()

#######################################################
#                   Taxa de Informalidade             #
#######################################################

item8 <- basededados %>%
  group_by(Tempo) %>%
  mutate(aux1 = sum(informais), aux2 = sum(ocup.x),
         taxadeinformalidade = (aux1/aux2)*100) %>%
  summarise(taxadeinformalidade = mean(taxadeinformalidade))

Figura8 <- ggplot(data = item8, aes(Tempo, taxadeinformalidade)) +
  geom_line(color = "gray20") + 
  geom_point(shape = 21, color = "black", fill = "indianred1", size = 3) +
  geom_vline(xintercept = item1$Tempo[33], linetype = 8) +
  theme_bw() +
  labs(x = "Trimestre",
       y = "Em %") +
  theme(plot.title = element_text(family = "Times"))

plot(Figura8)

setwd(out_dir)
png("Evolucao_da_Informalidade.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura8)
dev.off()

#######################################################
#               Evolução dos Nem-nens                 #
#######################################################

item9 <- basededados %>%
  group_by(Tempo) %>%
  mutate(aux1 = sum(nemnem), aux2 = sum(jovens),
         taxanemnem = (aux1/aux2)*100) %>%
  summarise(taxanemnem = mean(taxanemnem))

Figura9 <- ggplot(data = item9, aes(Tempo, taxanemnem)) +
  geom_line(color = "gray20") + 
  geom_point(shape = 21, color = "black", fill = "indianred1", size = 3) +
  geom_vline(xintercept = item9$Tempo[33], linetype = 8) +
  theme_bw() +
  labs(x = "Trimestre",
       y = "Em %") +
  theme(plot.title = element_text(family = "Times"))

plot(Figura9)


setwd(out_dir)
png("Evolucao_dos_Nem-nens.png", units = "px", width = 850, height = 536, res = 110)
plot(Figura9)
dev.off()

#######################################################
#                   Taxa de Desocupacao               #
#######################################################


item1 <- basededados %>%
  group_by(Tempo) %>%
  mutate(aux1 = sum(desocup), aux2 = sum(PEA),
         taxadesocupacao = (aux1/aux2)*100) %>%
  summarise(taxadesocupacao = mean(taxadesocupacao))

windowsFonts(Times=windowsFont("Times New Roman"))

Figura10 <- ggplot(data = item1, aes(Tempo, taxadesocupacao)) +
  geom_line(color = "gray20") + 
  geom_point(shape = 21, color = "black", fill = "indianred1", size = 3) +
  geom_vline(xintercept = item1$Tempo[32], linetype = 8) +
  theme_bw() +
  labs(x = "Trimestre",
       y = "Em %",
       title = "Evolução da Taxa de Desocupação no Brasil") +
  theme(plot.title = element_text(family = "Times"))

setwd(out_dir)

png("Evolucao_da_Desocupacao.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura10)
dev.off()

#######################################################
#                   Taxa de Desalento                 #
#######################################################

item11 <- basededados %>%
  group_by(Tempo) %>%
  mutate(aux1 = sum(desalentados), aux2 = sum(PIA),
         taxadesalentados = (aux1/aux2)*100) %>%
  summarise(taxadesalentados = mean(taxadesalentados))

windowsFonts(Times=windowsFont("Times New Roman"))

Figura11 <- ggplot(data = item11, aes(Tempo, taxadesalentados)) +
  geom_line(color = "gray20") + 
  geom_point(shape = 21, color = "black", fill = "indianred1", size = 3) +
  geom_vline(xintercept = item11$Tempo[33], linetype = 8) +
  theme_bw() +
  labs(x = "Trimestre",
       y = "Em %") +
  theme(plot.title = element_text(family = "Times"))

plot(Figura11)

setwd(out_dir)
png("Evolucao_do_Desalento.png", units = "px", width = 850, height = 536, res = 110)
plot(Figura11)
dev.off()
