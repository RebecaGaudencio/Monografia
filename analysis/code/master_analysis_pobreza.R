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
         aux7 = sum(PobrezaPretosPardos),
         aux8 = sum(PretosePardos),
         Pbrancos = ((aux1/aux2)*100),
         Ppardos = ((aux3/aux4)*100),
         Ppretos = ((aux5/aux6)*100),
         Ppretosepardos = ((aux7/aux8)*100)) %>%
  summarise(Pbrancos = mean(Pbrancos),
            Ppardos = mean(Ppardos),
            Ppretos = mean(Ppretos),
            Ppretosepardos = mean(Ppretosepardos))


item5 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(PobrezaHomemBranco),
         aux2 = sum(Homensbrancos),
         aux3 = sum(PobrezaHomemPardo),
         aux4 = sum(Homenspardos),
         aux5 = sum(PobrezaHomemPreto),
         aux6 = sum(Homenspretos),
         aux7 = sum(PobrezaHomemPretoePardo),
         aux8 = sum(Homenspretosepardos),
         aux9 = sum(PobrezaMulherBranca),
         aux10 = sum(Mulheresbrancas),
         aux11 = sum(PobrezaMulherParda),
         aux12 = sum(Mulherespardas),
         aux13 = sum(PobrezaMulherPreta),
         aux14 = sum(Mulherespretas),
         aux15 = sum(PobrezaMulherPretaeParda),
         aux16 = sum(Mulherespretasepardas), 
         PHomembranco = ((aux1/aux2)*100),
         PHomempardo = ((aux3/aux4)*100),
         PHomempreto = ((aux5/aux6)*100),
         PHomempretoepardo = ((aux7/aux8)*100),
         PMulherbranca = ((aux9/aux10)*100),
         PMulherparda = ((aux11/aux12)*100),
         PMulhernegra = ((aux13/aux14)*100),
         PMulherpretaaeparda = ((aux15/aux16)*100)) %>%
  summarise(PHomembranco = mean(PHomembranco),
            PHomempardo = mean(PHomempardo),
            PHomempreto = mean(PHomempreto),
            PHomempretoepardo = mean(PHomempretoepardo),
            PMulherbranca = mean(PMulherbranca),
            PMulherparda = mean(PMulherparda),
            PMulhernegra = mean(PMulhernegra),
            PMulherpretaaeparda = mean(PMulherpretaaeparda))


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
         Pgrupo1 = ((aux1/aux2)*100),
         Pgrupo2 = ((aux3/aux4)*100),
         Pgrupo3 = ((aux5/aux6)*100),
         Pgrupo4 = ((aux7/aux8)*100)) %>%
  summarise(Pgrupo1 = mean(Pgrupo1),
            Pgrupo2 = mean(Pgrupo2),
            Pgrupo3 = mean(Pgrupo3),
            Pgrupo4 = mean(Pgrupo4))


###########################################################
#                    Composição da Renda                  #
###########################################################
basededados[is.na(basededados)] <- 0

item7 <- basededados %>%
  group_by(Ano) %>%
  mutate(outros = RendaPobres - (RendaPobresTrabalho + RendaPobresBPC + RendaPobresBF + RendaPobresPSocial +
                                  RendaPobresSegdesemprego + RendaPobresAposentadoria + RendaPobresDoacao + RendaPobresAluguel), 
         aux1 = sum(RendaPobres),
         aux2 = sum(RendaPobresTrabalho),
         aux3 = sum(RendaPobresBPC),
         aux4 = sum(RendaPobresBF),
         aux5 = sum(RendaPobresPSocial),
         aux6 = sum(RendaPobresSegdesemprego),
         aux7 = sum(RendaPobresAposentadoria),
         aux8 = sum(RendaPobresDoacao),
         aux9 = sum(RendaPobresAluguel),
         aux10 = sum(outros),
         Poutrasfontes = ((aux10/aux1)*100),
         Ptrabalho = ((aux2/aux1)*100),
         Pajuda = (((aux3+aux4+aux5+aux6)/aux1)*100),
         Papoaluguel = (((aux7+aux9)/aux1)*100),
         Pdoacao = ((aux8/aux1)*100)) %>%
  summarise("Outras Fontes" = mean(Poutrasfontes),
            "Trabalho" = mean(Ptrabalho),
            "Ajuda Governamental" = mean(Pajuda),
            "Aposentadoria e Aluguel" = mean(Papoaluguel),
            "Doação" = mean(Pdoacao))

item8 <- item7 %>%
  dplyr::filter(Ano == 2019) %>%
  pivot_longer(cols = c("Outras Fontes",
                        "Trabalho", 
                        "Ajuda Governamental", 
                        "Aposentadoria e Aluguel", 
                        "Doação"), 
               names_to = "Fonte")

colnames(item8) = c("Ano", "Fonte","Percentual")

Figura8 <- item8 %>%
  ggplot(aes(x = "", y = Percentual, fill = Fonte)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("coral4", "coral2", "tan1", "tan", "black"))+
  labs(x = "Em %",
       y = "2019",
       title = "Decomposição da Renda - Renda de pessoas pobres")

plot(Figura8)

setwd(out_dir)
png("Decomposição_Renda_Pobres.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura8)
dev.off()


item9 <- basededados %>%
  group_by(Ano) %>%
  mutate(outros = rendatotal - (RendaTrabalho + RendaBPC + RendaBF + 
                                  RendaPSocial + RendaSegdesemprego + 
                                  RendaAposentadoria + RendaDoacao + 
                                  RendaAluguel),
         aux1 = sum(rendatotal),
         aux2 = sum(RendaTrabalho),
         aux3 = sum(RendaBPC),
         aux4 = sum(RendaBF),
         aux5 = sum(RendaPSocial),
         aux6 = sum(RendaSegdesemprego),
         aux7 = sum(RendaAposentadoria),
         aux8 = sum(RendaDoacao),
         aux9 = sum(RendaAluguel),
         aux10 = sum(outros),
         Routrasfontes = ((aux10/aux1)*100),
         Rtrabalho = ((aux2/aux1)*100),
         Rajuda = (((aux3+aux4+aux5+aux6)/aux1)*100),
         Rapoaluguel = (((aux7+aux9)/aux1)*100),
         Rdoacao = ((aux8/aux1)*100),
         Soma = (((aux2+aux3+aux4+aux5+aux6+aux7+aux8+aux9+aux10)/aux1)*100))%>%
  summarise("Outras Fontes" = mean(Routrasfontes),
            "Trabalho" = mean(Rtrabalho),
            "Ajuda Governamental" = mean(Rajuda),
            "Aposentadoria e Aluguel" = mean(Rapoaluguel),
            "Doação" = mean(Rdoacao))

item10 <- item9 %>%
  dplyr::filter(Ano == 2019) %>%
  pivot_longer(cols = c("Outras Fontes",
                        "Trabalho", 
                        "Ajuda Governamental", 
                        "Aposentadoria e Aluguel", 
                        "Doação"), 
               names_to = "Fonte")

colnames(item10) = c("Ano","Fonte","Percentual")

Figura9 <- item10 %>%
  ggplot(aes(x = "", y = Percentual, fill = Fonte)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("coral4", "coral2", "tan1", "tan", "black"))+
  labs(x = "Em %",
       y = "2019",
       title = "Decomposição da Renda - Renda Total")

plot(Figura9)

setwd(out_dir)
png("Decomposição_Renda_Todos.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura9)
dev.off()

###########################################################
#                     Hiato da Pobreza                    #
###########################################################

item11 <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = mean(HiatoRenda),
         aux2 = sum(HiatoAgregado),
         aux3 = sum(populacao)) %>%
  summarise(HRenda = mean(aux1),
            HAgregado = mean(aux2),
            Hagregadopop = mean(aux1/aux3))
  

Figura11 <- ggplot(item11, aes(Ano, HRenda)) +
  geom_line(color = "gray20") +
  geom_point(shape = 21, color = "black", fill = "indianred", size = 3) +
  theme_bw() +
  labs (x = "Ano",
        y = "Em R$") +
  theme(plot.title = element_text(family = "Times"))


setwd(out_dir)

png("Evolução_Média_Hiatos_Renda.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura11)
dev.off()


Figura12 <- ggplot(item11, aes(Ano, HAgregado)) +
  geom_line(color = "gray20") +
  geom_point(shape = 21, color = "black", fill = "indianred", size = 3) +
  theme_bw() +
  labs (x = "Ano",
        y = "Em R$") +
  theme(plot.title = element_text(family = "Times"))


setwd(out_dir)

png("Evolução_Hiato_Agregado.png", units = "px", width = 850, height = 536, res = 100)
plot(Figura12)
dev.off()



item <- basededados %>%
  group_by(Ano) %>%
  mutate(aux1 = sum(PobrezaBrasil),
         aux2 = sum(ExtremaPobrezaBrasil),
         Pobres = aux1,
         EPobres = aux2) %>%
  summarise(Pobres = mean(Pobres),
            EPobres = mean (EPobres))


