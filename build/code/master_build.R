####################################################################
#####                                                          #####
##                           Monografia                           ##
#####                                                          #####
####################################################################

install.packages("usethis")
install.packages("PNADcIBGE")
library(PNADcIBGE)

# Importação Online
help("get_pnadc")

input_path <- file.path("C:/Users/rebec/Documents/GitHub/Monografia/build/input")
setwd(input_path)
lista_PNAD = list.files(pattern = "PNADC_012012.txt")
chave_input = list.files(pattern = "input_PNADC_trimestral.sas")


pnadc_df = read_pnadc(microdata=lista_PNAD, input_txt = chave_input)


input_path <- pnadc_example(path="PNADC_012012.txt")
data_path <- pnadc_example(path="exampledata.txt")
pnadc.df <- read_pnadc(microdata=data_path, input_txt=input_path, vars=c("VD4001","VD4002"))


