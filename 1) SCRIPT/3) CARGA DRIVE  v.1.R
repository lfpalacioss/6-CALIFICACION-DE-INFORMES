
########################## ###
##    PARAMETROS Y SETEO   ####
########################## ###

# Install and load necessary packages
rm(list=ls())
source('1) SCRIPT/1) BASE_LF.R')

library(googlesheets4)
googlesheets4::gs4_auth()


##############################


###################### ###
##    CARGA DE DATOS   ####
###################### ###

# Seteo
URL <- "https://docs.google.com/spreadsheets/d/133Ey__k1hMVSVVG_1Ad4-AoBmr0cqcJVEcN-Ue9ugcs/edit?usp=sharing"
INPUT <- "1"
OUTPUT <- "2"


# Read the data
BD_INPUT <- read_sheet(URL, INPUT)

##########################


####################### ###
##    MANEJO DE DATOS   ####
####################### ###

# Trabajar data
BD_OUTPUT = BD_INPUT %>% 
  subset(select = c(1:4,8,11,18,29:31))


###########################


####################### ###
##    SUBIDA DE DATOS   ####
####################### ###

googlesheets4::write_sheet(BD_OUTPUT, 
                           ss = URL,
                           sheet = "2")

###########################



