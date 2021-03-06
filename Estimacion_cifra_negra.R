library(pacman)
p_load(dplyr,survey)

rm(list = ls())

setwd("/Users/HP/Documents/Descifra")

# Tablas que se usar�n para los c�lculos de Prevalencia Delictiva, Incidencia Delictiva y Cifra Negra
tpv1<-read.csv("cifra_negra/conjunto_de_datos_envipe2019_csv/conjunto_de_datos_TPer_Vic1_ENVIPE_2019/conjunto_de_datos/conjunto_de_datos_TPer_Vic1_ENVIPE_2019.csv",stringsAsFactors = F) # Tabla Principal de Victimizaci�n 1
tpv2<-read.csv("cifra_negra/conjunto_de_datos_envipe2019_csv/conjunto_de_datos_TPer_Vic2_ENVIPE_2019/conjunto_de_datos/conjunto_de_datos_TPer_Vic2_ENVIPE_2019.csv",stringsAsFactors = F) # Tabla Principal de Victimizaci�n 2
tmv <-read.csv("cifra_negra/conjunto_de_datos_envipe2019_csv/conjunto_de_datos_TMod_Vic_ENVIPE_2019/conjunto_de_datos/conjunto_de_datos_TMod_Vic_ENVIPE_2019.csv",stringsAsFactors = F) # Tabla M�dulo de Victimizaci�n

Entidades<-c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", "Baja California Sur", "Campeche", "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de M�xico", "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco","Estado de M�xico", "Michoac�n de Ocampo", "Morelos", "Nayarit", "Nuevo Le�n", "Oaxaca", "Puebla", "Quer�taro", "Quintana Roo", "San Luis Potos�", "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala","Veracruz de Ignacio de la Llave", "Yucat�n","Zacatecas","Entidad no especificada")



##### Valor	Categor�a: BDCOD####

# 01	Robo total de veh�culo (autom�vil, camioneta, cami�n)
# 02	Robo de accesorios, refacciones o herramientas de veh�culos (autom�vil, camioneta, cami�n)
# 03	Pinta de barda o grafiti en su casa, rayones intencionales en su veh�culo u otro tipo de vandalismo
# 04	Alguien entr� a su casa o departamento sin permiso mediante el uso de la fuerza o por enga�os y rob� o intent� robar algo
# 05	Robo o asalto en la calle o en el transporte p�blico (incluye robo en banco o cajero autom�tico)
# 06	Robo en forma distinta a la anterior
# 07	Alguien us� su chequera, n�mero de tarjeta o cuenta bancaria sin su permiso para realizar cargos o para extraer dinero de sus cuentas (fraude bancario) o le dio dinero falso
# 08	Entreg� dinero por un producto o un servicio que no recibi� conforme a lo acordado (fraude al consumidor)
# 09	Amenazas, presiones o enga�os para exigirle dinero o bienes; o para que hiciera algo o dejara de hacerlo (extorsi�n)
# 10	Amenazas verbales de alguien plenamente identificado o por escrito hacia su persona diciendo que le va a causar un da�o a usted, a su familia, a sus bienes o su trabajo
# 11	Alguien s�lo por actitud abusiva o por una discusi�n lo(a) golpe� gener�ndole una lesi�n f�sica (moretones, fracturas, cortadas, etc.)
# 12	Lo secuestraron para exigir dinero o bienes
# 13	Alguien en contra de su voluntad lo(a) agredi� mediante hostigamiento sexual, manoseo, exhibicionismo o intento de violaci�n
# 14	Fue obligado(a) mediante violencia f�sica o amenaza por alguien conocido o desconocido a tener una actividad sexual no deseada (violaci�n sexual)
# 15	Otros delitos distintos a los anteriores

#Selecci�n de delitos:

# 01	Robo total de veh�culo (autom�vil, camioneta, cami�n)

# 04	Alguien entr� a su casa o departamento sin permiso mediante el uso de la fuerza o por enga�os y rob� o intent� robar algo
# 05	Robo o asalto en la calle o en el transporte p�blico (incluye robo en banco o cajero autom�tico)
# 06	Robo en forma distinta a la anterior
# 07	Alguien us� su chequera, n�mero de tarjeta o cuenta bancaria sin su permiso para realizar cargos o para extraer dinero de sus cuentas (fraude bancario) o le dio dinero falso

# 09	Amenazas, presiones o enga�os para exigirle dinero o bienes; o para que hiciera algo o dejara de hacerlo (extorsi�n)

# 11	Alguien s�lo por actitud abusiva o por una discusi�n lo(a) golpe� gener�ndole una lesi�n f�sica (moretones, fracturas, cortadas, etc.) (lesiones)
# 12	Lo secuestraron para exigir dinero o bienes
# 13	Alguien en contra de su voluntad lo(a) agredi� mediante hostigamiento sexual, manoseo, exhibicionismo o intento de violaci�n (acoso sexual)
# 14	Fue obligado(a) mediante violencia f�sica o amenaza por alguien conocido o desconocido a tener una actividad sexual no deseada (violaci�n sexual)

# 15	Otros delitos distintos a los anteriores


#####Delitos ocurridos por entidad federativa, seg�n cifra negra#####

# Construcci�n de las variables
# Delitos Ocurridos
tmv$DO <- ifelse(!tmv$BPCOD%in%"03",1,0)

# Construcci�n del filtro de Cifra Negra
# Delitos Denunciados
tmv$DD <- ifelse(!tmv$BPCOD%in% "03" & (tmv$BP1_20== 1 | tmv$BP1_21%in%"1\n"),1,0)
# Delitos No Denunciados
tmv$DND <- ifelse(!tmv$BPCOD%in% "03" & (!tmv$BP1_20== 1  &
                                     !tmv$BP1_21%in%c("1\n","9\n" )),1,0)
#Delitos Denunciados Sin Inicio de Averiguaci�n Previa/Carpeta de Investigaci�n
tmv$DSAP <- ifelse(tmv$DD==1 & (tmv$BP1_24 != "1\n"),1,0)
# Delitos Denunciados en los cuales no fue especificado si se inici�
# Averiguaci�n Previa/Carpeta de Investigaci�n
tmv$DNE <- ifelse(tmv$DD==1 & (tmv$BP1_22 == "2\n" | tmv$BP1_24 == "9\n"),1,0)
# Delitos en los cuales no fue especificado si se denunci�
tmv$NE <- ifelse(tmv$BPCOD == 5 & tmv$BP1_21 == "9\n",1,0)

# Construcci�n de la variable de Cifra Negra
tmv$CN <- ifelse(tmv$DND%in%"1"|tmv$DSAP%in%"1"|tmv$DNE%in%"1"|tmv$NE%in%"1",1,0)

# Construcci�n de la variable que especifica el dise�o de la encuesta
DIS <- svydesign(id=~UPM_DIS, strata=~EST_DIS, data=tmv, weights=~FAC_DEL)

# C�lculo de Delitos Ocurridos por Entidad Federativa por delito
DOE <- svyby(~DO,by = ~BP1_2C+BPCOD,DIS,svytotal)

# C�lculo de cifra negra por Entidad Federativa por delito
CNE <- svyby(~CN,by = ~BP1_2C+BPCOD,DIS,svytotal)

# C�lculo de delitos denunciados por Entidad Federativa por delito
DDE <- svyby(~DD,by = ~BP1_2C+BPCOD,DIS,svytotal)

# C�lculo de Cifra Negra por Entidad Federativa (Relativos) 
RCNE <- svyby(~CN,denominator=~DO,by = ~BP1_2C+BPCOD,DIS,svyratio)

# C�lculo de delitos ocurrido por Entidad Federativa (Relativos) Este es el valor del deflactor a dividir para estimar los delitos ocurridos por tipo de delito
RDDE <- svyby(~DD,denominator=~DO,by = ~BP1_2C+BPCOD,DIS,svyratio)
