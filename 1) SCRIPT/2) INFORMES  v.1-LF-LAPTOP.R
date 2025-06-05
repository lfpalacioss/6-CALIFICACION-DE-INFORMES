######################################################
######################################################
######################################################


########################## ###
##    PARAMETROS Y SETEO   ####
########################## ###

rm(list=ls())
source('1) SCRIPT/1) BASE_LF.R')
RES=250
ALTO=1400 #1000
ANCHO=2600 #1200
ESCAL_CONV=0.026458333

# RUTA_STOCK = "2) BDs/4) REPORTES/1) INAF/1) STOCK/"
# RUTA_DOCUMENTOS = "2) BDs/4) REPORTES/1) INAF/2) DOCUMENTOS/"

#Actualizar los accesos
library(googlesheets4)
googlesheets4::gs4_auth("analisisdedatos-dfai@oefa.gob.pe")

#Funciones creadas: Eliminar tilder y caracteres raros
DEL_TILD <- function( s ) {
  chartr("áéóūáéíóúÁÉÍÓÚýÝàèìòùÀÈÌÒÙâêîôûÂÊÎÔÛãõÃÕñÑäëïöüÄËÏÖÜÿçÇ",
         "aeouaeiouAEIOUyYaeiouAEIOUaeiouAEIOUaoAOnNaeiouAEIOUycC",
         s );
}


#Funciones creadas: Crear calendario
FERIADOS = c("2023-01-01", "2023-01-02", "2023-04-06", "2023-04-07", "2023-05-01",
             "2023-06-29", "2023-07-23", "2023-07-28", "2023-07-29", "2023-08-06", 
             "2023-08-30", "2023-10-08", "2023-11-01", "2023-12-07", "2023-12-08", 
             "2023-12-09", "2023-12-25", "2023-12-26"
             
             )

CAL_2023 = bizdays::create.calendar("CAL_2023", 
                                    weekdays = c("saturday", "sunday"),
                                    holidays = FERIADOS)


# #Función para el calculo del tamaño de muestra por proporciones
# T_MUESTRA_P = function(N, p , conf, e) {
#   ceiling((N*p*(1-p))/((N-1)*(e^2)/(qnorm(1-(1-conf)/2,0,1)^2)+p*(1-p)))
# }
# 
# #Parámetros estandar para proporciones
# P = 0.5           # % de éxito
# CONF = 0.95       # Nivel de confianza
# E = 0.07          # Error máximo permitible
# T_NR = 0.0       # Tasa de no respuesta



##############################


# ############################### ###
# ##    BD INGRESO DE INFORMES    ####
# ############################### ###
# 
# BD_DOCS = read_xlsx(path = paste0("C:/Users/lfpal/OneDrive/1) TRABAJO/2) OEFA/1) DFAI/1) PROYECTO BD/2) BDs/4) REPORTES/1) INAF/2) DOCUMENTOS/","DOCUMENTOS.xlsx"), 
#                     sheet = "DOCS",
#                     skip = 8,
#                     col_types = c(rep("guess",5),
#                                   "text", "text",
#                                   rep("guess",23))
#                     ) %>% 
#   rename(NUM = 1,
#          NUM_DOC = 2,
#          NUM_EXP = 3,
#          F_EMI = 4,
#          T_DOC = 5,
#          SENTIDO = 6,
#          F_NOTIF = 7,
#          PROCED = 8,
#          T_PAS = 9,
#          REINCID = 10,
#          SUBSECT = 11,
#          T_INF = 12,
#          NUM_INF_SUP = 13,
#          F_INI_SUP = 14,
#          COD_ADM = 16,
#          ADM = 17,
#          COD_UF = 18,
#          UF = 19,
#          DPTO = 20,
#          PROV = 21,
#          DIST = 22,
#          EQUIPO = 23,
#          DOC_JEFE_EQ = 24,
#          JEFE_EQ = 25,
#          DOC_ANALIST = 26,
#          ANALIST = 27,
#          F_PRESC = 28,
#          OBS = 29,
#          F_REG = 30)
# 
# BD_DOCS_2 = BD_DOCS %>% 
#   distinct(NUM_INF_SUP,.keep_all = T) %>% 
#   subset()
# 
# 
# 
# ###################################


################################ ###
##    BD REVISION DE INFORMES    ####
################################ ###

# Seteo
URL <- "https://docs.google.com/spreadsheets/d/1le6ts3LMrmuBR5art1LIPVobWfwUOEP2GsajU8gpt5c/edit?usp=sharing"
INPUT <- "RESPUESTAS"
# OUTPUT <- "2"


# Read the data
BD_INF = read_sheet(URL, INPUT) %>% 
  rename(F_REG = 1,
         CORREO = 2,
         APELL = 3,
         NOMB = 4,
         DNI = 5,
         COD_HT = 6,
         NUM_INF = 7,
         
         SUBSECT = 8,
         OD_ORIG = 9,
         CRIT_1 = 10,
         CRIT_2 = 11,
         CRIT_3 = 12,
         CRIT_4 = 13,
         CRIT_5 = 14,
         CRIT_6 = 15,
         CRIT_7 = 16,
         
         CALIF_FORM = 17,
         DICTAMEN_FORM = 18,
         COMENT_FORM = 19,
         
         CALIF_FOND = 20,
         COMENT_FOND = 21,
         DICTAMEN_FOND = 22,

         
         CRIT_1_BOOL = 23,
         CRIT_2_BOOL = 24,
         CRIT_3_BOOL = 25,
         CRIT_4_BOOL = 26,
         CRIT_5_BOOL = 27,
         CRIT_6_BOOL = 28,
         CRIT_7_BOOL = 29,
         
         SCORE_FORM = 30,
         SCORE_FOND = 31,
         SCORE_TOT = 32,
         RESULT = 33) %>%
  
  mutate(F_REG = as.Date(floor_date(F_REG, unit = "day")),
         AREA = case_when(SUBSECT == "Minería" | SUBSECT == "Hidrocarburos mayores" | SUBSECT == "Hidrocarburos menores" | SUBSECT == "Electricidad" ~ "SFEM",
                          SUBSECT == "Agricultura" | SUBSECT == "Pesca" | SUBSECT == "Industria" | SUBSECT == "Consultoras ambientales" ~ "SFAP",
                          SUBSECT == "Residuos sólidos" ~ "SFIS"),
         ACEPT = case_when(DICTAMEN_FORM == "Sin dictamen" ~ "Sin registro",
                           DICTAMEN_FORM == "Calidad aceptable" ~ "Aceptable",
                           DICTAMEN_FORM == "Calidad inaceptable" ~ "Inaceptable"))
  
 


####################################


############### ###
##    TABLAS    ####
############### ###

# #Meta
# img = jpeg::readJPEG("2) INPUT/META MARZO.jpg")
# plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", xlim = 0:1, ylim = 0:1) #create an empty plot
# rasterImage(img, 0, 0, 1, 1)

#General (Área)
TAB_INF_G = BD_INF %>% 
  mutate(MES_REG = factor(month(F_REG),
                          levels = 1:12,
                          labels = c("Enero", "Febrero", "Marzo", 
                                     "Abril", "Mayo", "Junio", "Julio", 
                                     "Agosto", "Septiembre", "Octubre", 
                                     "Noviembre", "Diciembre"))) %>% 
  group_by(MES_REG, AREA) %>% 
  summarise(CANT = n()) %>% 
  pivot_wider(
    id_cols = c(AREA),
    names_from = MES_REG,
    values_from = c(CANT)
  ) %>% 
  ungroup() %>% 
  rowwise() %>%
  
  mutate(TOTAL = sum(c_across(c(2,3)), na.rm = TRUE)) %>%
  
  
  
  
  kableExtra::kable() %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::save_kable(file = "3) OUTPUT//2) TABLAS//TAB_1.1.html")




#Específico (Área+subsect)
TAB_INF_E = BD_INF %>% 
  mutate(MES_REG = factor(month(F_REG),
                          levels = 1:12,
                          labels = c("Enero", "Febrero", "Marzo", 
                                     "Abril", "Mayo", "Junio", "Julio", 
                                     "Agosto", "Septiembre", "Octubre", 
                                     "Noviembre", "Diciembre"))) %>% 
  group_by(MES_REG, AREA, SUBSECT) %>% 
  summarise(CANT = n()) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = c(AREA, SUBSECT),
    names_from = MES_REG,
    values_from = c(CANT)
  ) %>% 
  rowwise() %>%
  mutate(TOTAL = sum(across(c(3,4)), na.rm = TRUE)) %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::save_kable(file = "3) OUTPUT//2) TABLAS//TAB_1.2.html")



#Calif
TAB_CALIF_G = BD_INF %>% 
  summarise("Calificación Criterio 1" = sum(CRIT_1_BOOL, na.rm = T)/n(),
            "Calificación Criterio 2" = sum(CRIT_2_BOOL, na.rm = T)/n(),
            "Calificación Criterio 3" = sum(CRIT_3_BOOL, na.rm = T)/n(),
            "Calificación Criterio 4" = sum(CRIT_4_BOOL, na.rm = T)/n(),
            "Calificación Criterio 5" = sum(CRIT_5_BOOL, na.rm = T)/n(),
            "Calificación Criterio 6" = sum(CRIT_6_BOOL, na.rm = T)/n(),
            "Calificación Criterio 7" = sum(CRIT_7_BOOL, na.rm = T)/n(),
            "Cantidad de informes" = n(),
            "Calificación mínima" = min(CALIF_FORM, na.rm = T),
            "Calificación máxima" = max(CALIF_FORM, na.rm = T),
            "Calificación promedio" = mean(CALIF_FORM, na.rm = T),
            "Score mínimo" = min(SCORE_FORM, na.rm = T),
            "Score máximo" = max(SCORE_FORM, na.rm = T),
            "Score promedio" = mean(SCORE_FORM, na.rm = T)) %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::save_kable(file = "3) OUTPUT//2) TABLAS//TAB_2.1.html")



TAB_CALIF_E = BD_INF %>% 
  group_by(AREA, SUBSECT) %>% 
  summarise("Calificación Criterio 1" = percent(sum(CRIT_1_BOOL, na.rm = T)/n(), digits = 1),
            "Calificación Criterio 2" = percent(sum(CRIT_2_BOOL, na.rm = T)/n(), digits = 1),
            "Calificación Criterio 3" = percent(sum(CRIT_3_BOOL, na.rm = T)/n(), digits = 1),
            "Calificación Criterio 4" = percent(sum(CRIT_4_BOOL, na.rm = T)/n(), digits = 1),
            "Calificación Criterio 5" = percent(sum(CRIT_5_BOOL, na.rm = T)/n(), digits = 1),
            "Calificación Criterio 6" = percent(sum(CRIT_6_BOOL, na.rm = T)/n(), digits = 1),
            "Calificación Criterio 7" = percent(sum(CRIT_7_BOOL, na.rm = T)/n(), digits = 1),
            "Cantidad de Informes" = n(),
            "Resultado promedio" = percent(mean(SCORE_FORM, na.rm = T), digits = 1)) %>% 
  kableExtra::kable() %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::save_kable(file = "3) OUTPUT//2) TABLAS//TAB_2.2.html")






###################


############################ ###
##    GRAFICOS Y ANALISIS    ####
############################ ###

#Evol
G_1_General = BD_INF %>%
  mutate(semana = lubridate::week(F_REG),
         mes = month(F_REG),
         mes_aux = format(F_REG, "%b"),
         sem_mes = ceiling(day(F_REG)/7),
         sem_mes_aux = paste0("Semana ",
                              sem_mes,
                              " ",
                              mes_aux),
         F_SEMANA = floor_date(F_REG, "week", week_start = 1)) %>% 
  group_by(AREA,F_SEMANA) %>% 
  summarise(Q = n()) %>% 
  
  ggplot(aes(x = F_SEMANA,
             y = Q,
             color = AREA)) + 
  scale_color_manual(values = c("blue", "red", "grey"))+
  
  geom_line() +
  geom_point() +
  labs(title = "Cantidad de informes evaluados, por semana",
       y = "Cantidad de informes",
       x = "Fecha de registro",
       color = "Área",
       caption = "") +
  theme_bw()

G_1_General

ggsave("3) OUTPUT//3) GRAFICOS//G_1_General.jpg", width = 8, height = 6)



G_1_Especif = BD_INF %>%
  mutate(semana = lubridate::week(F_REG),
         mes = month(F_REG),
         mes_aux = format(F_REG, "%b"),
         sem_mes = ceiling(day(F_REG)/7),
         sem_mes_aux = paste0("Semana ",
                              sem_mes,
                              " ",
                              mes_aux),
         F_SEMANA = floor_date(F_REG, "week", week_start = 1)) %>% 
  group_by(SUBSECT, F_SEMANA) %>% 
  summarise(Q = n()) %>% 
  
  ggplot(aes(x = F_SEMANA,
             y = Q,
             color = SUBSECT)) + 
  scale_color_manual(values = c(PALETA.PRINCIPAL, PALETA.SECUNDARIA))+
  geom_line() +
  geom_point() + 
  labs(title = "Cantidad de informes evaluados, por semana",
       y = "Cantidad de informes",
       x = "Fecha de registro",
       color = "Subsector",
       caption = "") +
  theme_bw()

G_1_Especif

ggsave("3) OUTPUT//3) GRAFICOS//G_1_Especif.jpg", width = 8, height = 6)



#Scater plot
G_2 = BD_INF %>% 
  ggplot(aes(x = SCORE_FORM,
             y = CALIF_FORM,
             color = ACEPT)) + 
  scale_color_manual(values = c("blue", "red", "grey"))+
  
  geom_point(size = 3) +
  theme_bw() +
  facet_wrap(~AREA)+
  labs(x = "Score herramienta [0-1]",
       y = "Calificación evaluador [1-5]",
       color = "Dictamen evaluador")


G_2

ggsave("3) OUTPUT//3) GRAFICOS//G_2_Calif_gral.jpg", width = 8, height = 6)

  

G_3 = BD_INF %>% 
  ggplot(aes(x = SCORE_FORM,
             y = CALIF_FORM,
             color = ACEPT)) + 
  scale_color_manual(values = c("blue", "red", "grey"))+
  geom_point(size = 3)+
  theme_bw() +
  facet_wrap(~ SUBSECT)+
  labs(x = "Score herramienta [0-1]",
       y = "Calificación evaluador [1-5]",
       color = "Dictamen evaluador")

G_3

ggsave("3) OUTPUT//3) GRAFICOS//G_2_Calif_especif.jpg", width = 8, height = 6)



################################


######################### ###
##    ENVIO DE CORREOS    ####
######################### ###

#Actualizar el Token
library("gmailr")
path_old <- "~/Downloads/client_secret_835842225840-p2jabpue2al2gjjimg5for7apbskgsm3.apps.googleusercontent.com.json"
d <- fs::dir_create(rappdirs::user_data_dir("gmailr"), recurse = TRUE)
fs::file_move(path_old, d)

rappdirs::user_data_dir("gmailr") %>% 
  list.files()

gm_auth_configure()
gm_oauth_client()
gm_auth("analisisdedatos-dfai@oefa.gob.pe")


#Cargar destinatarios
DESTINATARIOS = list(DFAI = list("analisisdedatos-dfai@oefa.gob.pe", 
                                 "analisiseconomico-dfai@oefa.gob.pe",
                                 "coordinacioneconomica-dfai@oefa.gob.pe",
                                 "jurteaga@oefa.gob.pe",
                                 "glavalle@oefa.gob.pe",
                                 "kbanos@oefa.gob.pe",
                                 "elopezj@oefa.gob.pe"),
                     SFIS = list("lfpalacioss@hotmail.com"),
                     SFEM = list("lfpalacioss@pucp.edu.pe"),
                     SFAP = list("lfpalacioss@uni.pe"),
                     PRUEBAS = list("lfpalacioss@uni.pe"))


#Renderizar (crear) los archivos
ARCHIVO_REPORTE = rmarkdown::render("3) OUTPUT//1) CORREO//Archivo-Reporte.Rmd",
                                    # output_dir = "3) Html (Output)",
                                    output_file = "Archivo-Reporte.html")


#Convertir archivo en PDF
pagedown::chrome_print("3) OUTPUT//1) CORREO//Archivo-Reporte.html", 
                       "3) OUTPUT//1) CORREO//Archivo-Reporte.pdf")


#Crear el correo y enviar
msg = gm_mime() %>% 
  gm_to((DESTINATARIOS$DFAI)) %>%
  gm_from("analisisdedatos-dfai@oefa.gob.pe") %>% 
  gm_subject("📌📊 Avance del llenado de Informes de supervisión (REPORTE AUTOMATIZADO)") %>%
  gm_text_body("Estimado equipo, mediante el presente correo se adjunta la versión de prueba del primer reporte estadístico de los actos administrativos emitidos y pendientes por parte de la DFAI.\n\nCabe resaltar que esta es una versión de prueba, basado en la información cargada en los reporte de INAF, por lo cual los datos deben tomarse con prudencia, sin embargo, del primer testeo, se observa que la herramienta presenta un error de alrededor del 2%.\n\nSaludos Cordiales.\n\n-----------------------------\nLuis Felipe") %>%
  gm_attach_file("3) OUTPUT/1) CORREO/Archivo-Reporte.pdf")

gm_send_message(msg)

#############################

























  group_by(AÑO, MES) %>%
  summarise(HOR_SIN_SIST = sum(HOR_SIN_SIST, na.rm = T),
            HOR_TOT = sum(HOR_TOT, na.rm = T)) %>% 
  mutate(F_AUX = make_date(AÑO, MES, 01),
         CSA = HOR_SIN_SIST/HOR_TOT) %>% 
  mutate(PAND = ifelse(F_AUX >= "2020-03-01" & F_AUX <= "2021-04-01", 1,0))



graf_CSA = DATA_CSA %>% 
  ggplot(aes(x = F_AUX, y = CSA, group = NA))+
  geom_line(aes(color = "CSA"), size = 1, alpha = 0.8)+
  scale_color_manual(values = c("CSA" = PALETA.PRINCIPAL[1]))+
  geom_smooth(span = 0.5, aes(x = F_AUX, y = CSA),colour=PALETA.PRINCIPAL[3], fill = "gray80") +
  labs(x="",
       y="CSA",
       title = paste(""))+
  scale_y_continuous(labels=scales::percent_format(accuracy = NULL), limits = c(-0.005,0.075))+
  scale_x_date(labels = date_format("%b %y"), breaks = date_breaks("4 month"))+
  theme_minimal()+
  ggeasy::easy_remove_legend()+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")
graf_CSA

ggsave("3) OUTPUT/1) GRAFICOS/1) MKT/1.1) CSA-MKT.jpg",  width = 0.35*ANCHO*ESCAL_CONV, height = 0.30*ALTO*ESCAL_CONV, units="cm",dpi = RES)



graf_CSA_X = DATA_CSA %>% 
  subset(PAND == 0) %>% 
  ggplot(aes(x = F_AUX, y = CSA, group = NA))+
  geom_line(aes(color = "CSA"), size = 1, alpha = 0.8)+
  scale_color_manual(values = c("CSA" = PALETA.PRINCIPAL[1]))+
  geom_smooth(span = 0.5, aes(x = F_AUX, y = CSA),colour=PALETA.PRINCIPAL[3], fill = "gray80") +
  labs(x="",
       y="CSA",
       title = paste(""))+
  scale_y_continuous(labels=scales::percent_format(accuracy = NULL), limits = c(-0.005,0.01))+
  scale_x_date(labels = date_format("%b %y"), breaks = date_breaks("2 month"))+
  theme_minimal()+
  ggeasy::easy_remove_legend()+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")
graf_CSA_X

ggsave("3) OUTPUT/1) GRAFICOS/1) MKT/1.1) CSA-MKT-PAN.jpg",  width = 0.35*ANCHO*ESCAL_CONV, height = 0.30*ALTO*ESCAL_CONV, units="cm",dpi = RES)





# Densidad
graf_CSA2 = DATA_CSA %>% 
  ggplot()+
  geom_density(aes(x = CSA, alpha =0.75, color = "red", fill = "red"))+
  labs(x="",
       y="CSA",
       title = paste(""))+
  scale_x_continuous(labels=scales::percent_format(accuracy = NULL), 
                     limits = c(0,0.005),
                     breaks = seq(0, 0.005, by = 0.0005))+
  theme_minimal()+
  ggeasy::easy_rotate_x_labels(angle = 90,
                               side = "right")+
  ggeasy::easy_remove_legend()


graf_CSA2

ggsave("3) OUTPUT/1) GRAFICOS/1) MKT/1.2) CSA-KER-EMP.jpg",  width = 0.35*ANCHO*ESCAL_CONV, height = 0.30*ALTO*ESCAL_CONV, units="cm",dpi = RES)





#######################

















