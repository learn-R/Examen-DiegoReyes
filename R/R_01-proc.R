# Examen : Latinobarometro 2020

# 1. Cargar librerías


pacman::p_load(tidyverse, #Universo de paquetes
               sjmisc, #Para explorar datos
               dplyr, #Para manipular datos
               haven, #cargar y exportar bases de datos en formatos .sav y .dta
               car) #Para recodificar manteniendo las etiquetas


# 2. Cargar datos

datos_2020 <- read_rds("input/data/Latinobarometro_2020.rds")

# 3. Explorar datos
View(datos_2020)
head(datos_2020)

datos <- select(datos_2020, P13STGBS_B, p13st_c, p13st_d, p13st_e, edad, sexo, wt, s1)

# 4. seleccion de variable a utilizar 


P13STGBS.B : Confianza en la Policía / Carabineros
P13ST.C : Confianza en la Iglesia
P13ST.D : Confianza en el Congreso
P13ST.E : Confianza en el Gobierno
Edad
Sexo
wt : ponderador




# 5. Cambio de nombre de variables a utilizar
datos_proc <- datos_2020 %>% 
  dplyr::select(confianza_policia=P13STGBS_B,
                confianza_iglesia=p13st_c,
                confianza_congreso=p13st_d,
                confianza_gobierno=p13st_e,
                Clase_social=s1,
                Ponderador=wt,
                edad, sexo)

# 6. Recodificacion y codificacion de variables                 


datos_proc%>%

mutate_at(vars(sexo, confianza_policia, confianza_iglesia, confianza_congreso, confianza_gobierno), ~(as.factor(.))) %>% 
mutate(sexo = car::recode(.$sexo, recodes = c("1 = 'Hombre'; 2 = 'Mujer'")))%>% 

mutate(confianza_policia = car::recode(.$confianza_policia, recodes =c("1 = 'Mucho'; 2 = 'Algo';
                                            3 = 'Poca'; 4 = 'Ninguna'; c(-1, -2, -3, -4, -5) = NA"))) %>%
mutate(confianza_iglesia = car::recode(.$confianza_iglesia, recodes = c("1 = 'Mucho'; 2 = 'Algo';
                                            3 = 'Poca'; 4 = 'Ninguna'; c(-1, -2, -3, -4, -5) = NA"))) %>% 
mutate(confianza_congreso = car::recode(.$confianza_congreso, recodes = c("1 = 'Mucho'; 2 = 'Algo';
                                            3 = 'Poca'; 4 = 'Ninguna'; c(-1, -2, -3, -4, -5) = NA"))) %>%
mutate(confianza_gobierno = car::recode(.$confianza_gobierno, recodes =c("1 = 'Mucho'; 2 = 'Algo';
                                            3 = 'Poca'; 4 = 'Ninguna'; c(-1, -2, -3, -4, -5) = NA"))) %>%

mutate(Clase_social = car::recode(.$Clase_social, recodes =c("1 = 'Alta'; 2 = 'Media Alta'; 3 = 'Media';
                                         4 = 'Media Baja'; 5 = 'Baja'; c(-1, -2, -3, -4, -5) = NA"))) %>%
                                                              
  mutate(edad = case_when(edad>=18 & edad<=25~"Jovenes",
                          edad>=26 & edad<=40~"Adultos Jovenes",
                          edad>=41 & edad<=60~"Adultos",
                          edad>=60~"Adultos Mayores",
                          TRUE ~ NA_character_)) 

# 6.1 creacion objetos variables lista 

datos_proc <- datos_proc%>%
  
  mutate_at(vars(sexo,  confianza_policia, confianza_iglesia, confianza_congreso, confianza_gobierno), ~(as.factor(.))) %>% 
  mutate(sexo = car::recode(.$sexo, recodes = c("1 = 'Hombre'; 2 = 'Mujer'")))%>% 

  mutate(confianza_policia.rc = car::recode(.$confianza_policia, recodes =c("1 = 'Mucho'; 2 = 'Algo';
                                            3 = 'Poca'; 4 = 'Ninguna'; c(-1, -2, -3, -4, -5) = NA"))) %>%
  mutate(confianza_iglesia.rc = car::recode(.$confianza_iglesia, recodes = c("1 = 'Mucho'; 2 = 'Algo';
                                            3 = 'Poca'; 4 = 'Ninguna'; c(-1, -2, -3, -4, -5) = NA"))) %>% 
  mutate(confianza_congreso.rc = car::recode(.$confianza_congreso, recodes = c("1 = 'Mucho'; 2 = 'Algo';
                                            3 = 'Poca'; 4 = 'Ninguna'; c(-1, -2, -3, -4, -5) = NA"))) %>%
  mutate(confianza_gobierno.rc = car::recode(.$confianza_gobierno, recodes =c("1 = 'Mucho'; 2 = 'Algo';
                                            3 = 'Poca'; 4 = 'Ninguna'; c(-1, -2, -3, -4, -5) = NA"))) %>%
  mutate(edad = case_when(edad>=18 & edad<=25~"Jovenes",
                          edad>=26 & edad<=40~"Adultos Jovenes",
                          edad>=41 & edad<=60~"Adultos",
                          edad>=60~"Adultos Mayores",
                          TRUE ~ NA_character_)) 
# 6.2 Objeto final 
datos_proc <- datos_proc %>%
  mutate_at(vars( confianza_policia ,confianza_iglesia, confianza_congreso,
                 confianza_gobierno, sexo), ~(as.numeric(.))) # se tranforma a numeric para poder trabajar mas adelante con estas variables



  # 7. Exportar datos
saveRDS(datos_proc, file = "output/data/datos_proc.rds")
