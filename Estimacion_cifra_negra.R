library(pacman)
p_load(dplyr,survey)

rm(list = ls())

setwd("/Users/HP/Documents/Descifra")

# Tablas que se usarán para los cálculos de Prevalencia Delictiva, Incidencia Delictiva y Cifra Negra
tpv1<-read.csv("cifra_negra/conjunto_de_datos_envipe2019_csv/conjunto_de_datos_TPer_Vic1_ENVIPE_2019/conjunto_de_datos/conjunto_de_datos_TPer_Vic1_ENVIPE_2019.csv",stringsAsFactors = F) # Tabla Principal de Victimización 1
tpv2<-read.csv("cifra_negra/conjunto_de_datos_envipe2019_csv/conjunto_de_datos_TPer_Vic2_ENVIPE_2019/conjunto_de_datos/conjunto_de_datos_TPer_Vic2_ENVIPE_2019.csv",stringsAsFactors = F) # Tabla Principal de Victimización 2
tmv <-read.csv("cifra_negra/conjunto_de_datos_envipe2019_csv/conjunto_de_datos_TMod_Vic_ENVIPE_2019/conjunto_de_datos/conjunto_de_datos_TMod_Vic_ENVIPE_2019.csv",stringsAsFactors = F) # Tabla Módulo de Victimización

Entidades<-c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco","Estado de México", "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala","Veracruz de Ignacio de la Llave", "Yucatán","Zacatecas","Entidad no especificada")



##### Valor	Categoría: BDCOD####

# 01	Robo total de vehículo (automóvil, camioneta, camión)
# 02	Robo de accesorios, refacciones o herramientas de vehículos (automóvil, camioneta, camión)
# 03	Pinta de barda o grafiti en su casa, rayones intencionales en su vehículo u otro tipo de vandalismo
# 04	Alguien entró a su casa o departamento sin permiso mediante el uso de la fuerza o por engaños y robó o intentó robar algo
# 05	Robo o asalto en la calle o en el transporte público (incluye robo en banco o cajero automático)
# 06	Robo en forma distinta a la anterior
# 07	Alguien usó su chequera, número de tarjeta o cuenta bancaria sin su permiso para realizar cargos o para extraer dinero de sus cuentas (fraude bancario) o le dio dinero falso
# 08	Entregó dinero por un producto o un servicio que no recibió conforme a lo acordado (fraude al consumidor)
# 09	Amenazas, presiones o engaños para exigirle dinero o bienes; o para que hiciera algo o dejara de hacerlo (extorsión)
# 10	Amenazas verbales de alguien plenamente identificado o por escrito hacia su persona diciendo que le va a causar un daño a usted, a su familia, a sus bienes o su trabajo
# 11	Alguien sólo por actitud abusiva o por una discusión lo(a) golpeó generándole una lesión física (moretones, fracturas, cortadas, etc.)
# 12	Lo secuestraron para exigir dinero o bienes
# 13	Alguien en contra de su voluntad lo(a) agredió mediante hostigamiento sexual, manoseo, exhibicionismo o intento de violación
# 14	Fue obligado(a) mediante violencia física o amenaza por alguien conocido o desconocido a tener una actividad sexual no deseada (violación sexual)
# 15	Otros delitos distintos a los anteriores

#Selección de delitos:

# 01	Robo total de vehículo (automóvil, camioneta, camión)

# 04	Alguien entró a su casa o departamento sin permiso mediante el uso de la fuerza o por engaños y robó o intentó robar algo
# 05	Robo o asalto en la calle o en el transporte público (incluye robo en banco o cajero automático)
# 06	Robo en forma distinta a la anterior
# 07	Alguien usó su chequera, número de tarjeta o cuenta bancaria sin su permiso para realizar cargos o para extraer dinero de sus cuentas (fraude bancario) o le dio dinero falso

# 09	Amenazas, presiones o engaños para exigirle dinero o bienes; o para que hiciera algo o dejara de hacerlo (extorsión)

# 11	Alguien sólo por actitud abusiva o por una discusión lo(a) golpeó generándole una lesión física (moretones, fracturas, cortadas, etc.) (lesiones)
# 12	Lo secuestraron para exigir dinero o bienes
# 13	Alguien en contra de su voluntad lo(a) agredió mediante hostigamiento sexual, manoseo, exhibicionismo o intento de violación (acoso sexual)
# 14	Fue obligado(a) mediante violencia física o amenaza por alguien conocido o desconocido a tener una actividad sexual no deseada (violación sexual)

# 15	Otros delitos distintos a los anteriores


#####Delitos ocurridos por entidad federativa, según cifra negra#####

# Construcción de las variables
# Delitos Ocurridos
tmv$DO <- ifelse(!tmv$BPCOD%in%"03",1,0)

# Construcción del filtro de Cifra Negra
# Delitos Denunciados
tmv$DD <- ifelse(!tmv$BPCOD%in% "03" & (tmv$BP1_20== 1 | tmv$BP1_21%in%"1\n"),1,0)
# Delitos No Denunciados
tmv$DND <- ifelse(!tmv$BPCOD%in% "03" & (!tmv$BP1_20== 1  &
                                     !tmv$BP1_21%in%c("1\n","9\n" )),1,0)
#Delitos Denunciados Sin Inicio de Averiguación Previa/Carpeta de Investigación
tmv$DSAP <- ifelse(tmv$DD==1 & (tmv$BP1_24 != "1\n"),1,0)
# Delitos Denunciados en los cuales no fue especificado si se inició
# Averiguación Previa/Carpeta de Investigación
tmv$DNE <- ifelse(tmv$DD==1 & (tmv$BP1_22 == "2\n" | tmv$BP1_24 == "9\n"),1,0)
# Delitos en los cuales no fue especificado si se denunció
tmv$NE <- ifelse(tmv$BPCOD == 5 & tmv$BP1_21 == "9\n",1,0)

# Construcción de la variable de Cifra Negra
tmv$CN <- ifelse(tmv$DND%in%"1"|tmv$DSAP%in%"1"|tmv$DNE%in%"1"|tmv$NE%in%"1",1,0)

# Construcción de la variable que especifica el diseño de la encuesta
DIS <- svydesign(id=~UPM_DIS, strata=~EST_DIS, data=tmv, weights=~FAC_DEL)

# Cálculo de Delitos Ocurridos por Entidad Federativa por delito
DOE <- svyby(~DO,by = ~BP1_2C+BPCOD,DIS,svytotal)

# Cálculo de cifra negra por Entidad Federativa por delito
CNE <- svyby(~CN,by = ~BP1_2C+BPCOD,DIS,svytotal)

# Cálculo de delitos denunciados por Entidad Federativa por delito
DDE <- svyby(~DD,by = ~BP1_2C+BPCOD,DIS,svytotal)

# Cálculo de Cifra Negra por Entidad Federativa (Relativos) 
RCNE <- svyby(~CN,denominator=~DO,by = ~BP1_2C+BPCOD,DIS,svyratio)

# Cálculo de delitos ocurrido por Entidad Federativa (Relativos) Este es el valor del deflactor a dividir para estimar los delitos ocurridos por tipo de delito
RDDE <- svyby(~DD,denominator=~DO,by = ~BP1_2C+BPCOD,DIS,svyratio)
