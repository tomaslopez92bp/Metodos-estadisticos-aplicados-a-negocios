################################################################################
###################### PASOS PREVIOS ####################################
#remueve todos las variables cargadas en memoria
rm(list=ls())

#cambio el directorio sobre el que voy a trabajar
setwd("C:/Users/tlopez/Desktop/TLC/UTDT/MEAN/TP") 
getwd()

#install.packages
library("data.table")
library('dplyr')
library('lubridate')
library("ggplot2")
library('psych')
library('stats')
library('boot')

#vizualizacion de gráficos
theme_set(theme_bw()) # fondo blanco
options(scipen=999) # sin notación científica

################################################################################
###################### SUBIDA DE DATOS ####################################

# 1. Carga de Bases
usuarios <- read.csv("usuarios-ecobici-2018.csv")
recorridos <- read.csv("recorridos-realizados-2018.csv")

View(usuarios)
View(recorridos)

################################################################################
###################### TRABAJO DE LOS DATOS ####################################

#ordeno alfabeticamente y veo sus dimensiones
setcolorder(usuarios, sort(colnames(usuarios)))
dim(usuarios)
str(usuarios)

setcolorder(recorridos, sort(colnames(recorridos)))
dim(recorridos)
str(recorridos)

#1. Gráficos/tabulaciones de las principales estadisticas
#vemos variables promedio de todo el data set
summary(usuarios)
summary(recorridos)

#otro datos resumen
# en que estacion es mas y menos comun sacar y devolver la bici en la misma estacion
bicis.mismaest <- recorridos$id_estacion_origen == recorridos$id_estacion_destino
tablacruzada <- table(recorridos$nombre_estacion_origen, bicis.mismaest)
prop.table(tablacruzada)
which.max(prop.table(tablacruzada[,2]))
which.min(prop.table(tablacruzada)[,2])

#Modifico el tipo de dato de Integer a Date
class(recorridos$fecha_origen_recorrido)
typeof(recorridos$fecha_origen_recorrido)
recorridos$fecha_origen_recorrido <- as.POSIXct(recorridos$fecha_origen_recorrido, format="%Y-%m-%d %H:%M:%OS")

class(recorridos$fecha_destino_recorrido)
typeof(recorridos$fecha_destino_recorrido)
recorridos$fecha_destino_recorrido <- as.POSIXct(recorridos$fecha_destino_recorrido, format="%Y-%m-%d %H:%M:%OS")

#compruebo el cambio
class(recorridos$fecha_origen_recorrido)
class(recorridos$fecha_destino_recorrido)

#Modifico el tipo de datos ID de int y num a Factor y compruebo cambio
class(recorridos$id_estacion_destino)
typeof(recorridos$id_estacion_destino)
recorridos$id_estacion_destino <- as.factor(recorridos$id_estacion_destino)
class(recorridos$id_estacion_destino)

class(recorridos$id_estacion_origen)
typeof(recorridos$id_estacion_origen)
recorridos$id_estacion_origen <- as.factor(recorridos$id_estacion_origen)
class(recorridos$id_estacion_origen)

class(recorridos$id_usuario)
typeof(recorridos$id_usuario)
recorridos$id_usuario <- as.factor(recorridos$id_usuario)
class(recorridos$id_usuario)



#2. Estacionalidad por dia de semana del uso de las bicis

#creo dos nuevas columnas, Dia de la Semana y Fin de Semana
strftime(Sys.time(), format="%w")
recorridos$dia_semana = weekdays(recorridos$fecha_origen_recorrido)
recorridos$dia_semana_int = wday(recorridos$fecha_origen_recorrido)

#compruebo que el cambio fue correcto
head(recorridos$dia_semana)
head(recorridos$dia_semana_int)

prop.table(table(recorridos$dia_semana))
ggplot(as.data.frame(prop.table(table(recorridos$dia_semana))),aes(x=Var1,y=Freq)) + geom_bar(stat="identity")
#comprobamos que efectivamente podría existir estacionalidad entre dias de semana
#y fin de semana, donde el consumo es menor. Solo mirando el gráfico

#deberiamos hacer la vbla recorridos semana en numero y aplicar
head (recorridos$dia_semana_int)
table(recorridos$dia_semana_int)
prop.table(table(recorridos$dia_semana_int))

#vemos los gráficos
##creo contador por día de la semana
recorridos$yday = yday(recorridos$fecha_origen_recorrido) ##busco dia juliano
cantidad_dia_Semana = table(fecha = recorridos$yday, dia_semana= recorridos$dia_semana) ##creo tabla
cantidad_dia_Semana_df = as.data.frame(cantidad_dia_Semana) ##creo data frame asociado a la tabla

cantidad_dia_Semana_df = cantidad_dia_Semana_df[!(cantidad_dia_Semana_df$Freq == 0),] ##elimino 0/NA
arrange(cantidad_dia_Semana_df, fecha) ##ordeno data frame

#tests de hipotesis por dia de la semana y guardo sus intervalos
domingo_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "domingo"], conf.level = 0.95)$conf.int
sabado_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "sábado"], conf.level = 0.95)$conf.int
viernes_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "viernes"], conf.level = 0.95)$conf.int
jueves_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "jueves"], conf.level = 0.95)$conf.int
miercoles_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "miércoles"], conf.level = 0.95)$conf.int
martes_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "martes"], conf.level = 0.95)$conf.int
lunes_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "lunes"], conf.level = 0.95)$conf.int

#no vemos solapamiento entre día de semana y fin de semana
#existe estadisticamente estacionalidad entre dia de semana y fin de semana

intervalos_confianza <- data.frame(
  dia_semana = factor(c("domingo", "sábado", "viernes",
                        "jueves", "miércoles", "martes","lunes")),
  lowerIC = c(domingo_IC[1], sabado_IC[1], viernes_IC[1], jueves_IC[1], 
              miercoles_IC[1], martes_IC[1], lunes_IC[1]), 
  upperIC = c(domingo_IC[2], sabado_IC[2], viernes_IC[2], jueves_IC[2], 
              miercoles_IC[2], martes_IC[2], lunes_IC[2])
)

intervalos_confianza$dia_semana <- factor(intervalos_confianza$dia_semana,
                                          levels = c("domingo", "sábado", "viernes",
                                                     "jueves", "miércoles", "martes","lunes"))

#lo comprobamos ahora gráficamente
ggplot(data = intervalos_confianza, aes (y = dia_semana, x = upperIC, color = dia_semana))+
  geom_errorbar (aes(xmin = lowerIC, xmax = upperIC), width = 0.4)



#3. Eliminamos del punto anterior los dias feriados y re comprobamos

#creamos un vector con todos los días feriados
feriados = as.POSIXct(c("2018-01-01","2018-02-12","2018-02-13","2018-03-24","2018-03-29","2018-03-30","2018-04-02",
                        "2018-04-30","2018-05-01","2018-05-25","2018-06-20","2018-07-09","2018-08-17","2018-10-12",
                        "2018-11-20","2018-12-08","2018-12-24","2018-12-25","2018-12-31"), format="%Y-%m-%d")

#ahora creo una variable con los dias feriados y no feriados 
recorridos$feriado = factor(ifelse( format(recorridos$fecha_origen_recorrido,"%Y-%m-%d") %in% format(feriados,"%Y-%m-%d"), "Feriado" , "No Feriado"))
recorridos_sin_feriados = recorridos %>% filter(feriado=="No Feriado")

cantidad_dia_Semana = table(fecha = recorridos_sin_feriados$yday, dia_semana = recorridos_sin_feriados$dia_semana) ##creo tabla
cantidad_dia_Semana_df = as.data.frame(cantidad_dia_Semana) ##creo data frame asociado a la tabla

cantidad_dia_Semana_df = cantidad_dia_Semana_df[!(cantidad_dia_Semana_df$Freq == 0),] ##elimino 0/NA
arrange(cantidad_dia_Semana_df, fecha) ##ordeno data frame

#tests de hipotesis por dia de la semana y guardo sus intervalos
domingo_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "domingo"], conf.level = 0.95)$conf.int
sabado_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "sábado"], conf.level = 0.95)$conf.int
viernes_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "viernes"], conf.level = 0.95)$conf.int
jueves_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "jueves"], conf.level = 0.95)$conf.int
miercoles_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "miércoles"], conf.level = 0.95)$conf.int
martes_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "martes"], conf.level = 0.95)$conf.int
lunes_IC <- t.test(cantidad_dia_Semana_df$Freq[cantidad_dia_Semana_df$dia_semana == "lunes"], conf.level = 0.95)$conf.int

#no vemos solapamiento entre día de semana y fin de semana
#existe estadisticamente estacionalidad entre dia de semana y fin de semana
#sacando los casos extremos = fin de semana 

intervalos_confianza <- data.frame(
  dia_semana = factor(c("domingo", "sábado", "viernes",
                        "jueves", "miércoles", "martes","lunes")),
  lowerIC = c(domingo_IC[1], sabado_IC[1], viernes_IC[1], jueves_IC[1], 
              miercoles_IC[1], martes_IC[1], lunes_IC[1]), 
  upperIC = c(domingo_IC[2], sabado_IC[2], viernes_IC[2], jueves_IC[2], 
              miercoles_IC[2], martes_IC[2], lunes_IC[2])
)

intervalos_confianza$dia_semana <- factor(intervalos_confianza$dia_semana,
                                          levels = c("domingo", "sábado", "viernes",
                                                     "jueves", "miércoles", "martes","lunes"))

#lo comprobamos ahora gráficamente
ggplot(data = intervalos_confianza, aes (y = dia_semana, x = upperIC, color = dia_semana))+
  geom_errorbar (aes(xmin = lowerIC, xmax = upperIC), width = 0.4)



# 4. Usuario 606320
us606320 = recorridos %>% filter(id_usuario==606320 & id_estacion_origen!=id_estacion_destino) %>% select(lat_estacion_origen,long_estacion_origen,lat_estacion_destino,long_estacion_destino,duracion_recorrido)

write.csv(us606320, file="us606320.csv")
# Mediante Google Map se obtiene la distancian entre estaciones creando una nueva columna "recorrido_m"
us606320_dist = read.csv("us606320-distancias.csv")

us606320_dist$velocidad_min_seg = us606320_dist$recorrido_m/us606320_dist$duracion_seg

harmonic.mean(us606320_dist$velocidad_min_seg,na.rm=TRUE)
#la velocidad media harmónica es de 1.80 metros por segundo


#5. Desde "009-PARQUE LAS HERAS" hasta "066-BILLINGURST"
#proporción de usuarios que tardan más de 15 minutos no supera el 20%

#selecciono solo los recorridos bajo anáisis
recorrido9a66 = recorridos %>% filter(id_estacion_origen==9 & id_estacion_destino==66) %>% 
  select(id_usuario,id_estacion_origen,id_estacion_destino,duracion_recorrido)

recorrido9a66$duracion_recorrido_min = as.POSIXct(recorrido9a66$duracion_recorrido, format="0 days %H:%M:%OS")
recorrido9a66$duracion_recorrido_min =   
  (as.integer(format(recorrido9a66$duracion_recorrido_min,"%H"))*60)+#Horas a Minutos
  (as.integer(format(recorrido9a66$duracion_recorrido_min,"%M")))+#Minutos
  (as.integer(format(recorrido9a66$duracion_recorrido_min,"%H"))/60)#Segundos a Minutos

recorrido9a66$duracion_mayor_15min =  factor(ifelse(recorrido9a66$duracion_recorrido_min > 15, TRUE , FALSE))

prop.table(table(recorrido9a66$duracion_mayor_15min)) 
#Solo el 17,33% tarda mas de 15 minutos
prop_mas_15 <- 0.1733458

#test de hipotesis
# H0 = viaje mayor a 15 min >= 20%
# H1 = viaje mayor a 15 min < 20%

data = recorrido9a66 %>% filter(!is.na(duracion_recorrido_min)) %>% select(duracion_recorrido_min)
n = nrow(data)
p_sombrero <- prop_mas_15
zstat <- (p_sombrero - 0.2)/sqrt(0.2*0.8/n)
pvalor<- pnorm(zstat, lower.tail = TRUE)
pvalor
# pvalor = 0,01452683 < alfa = 0.05 --> RECHAZAMOS H0
#menos del 20% tarda más de 15 minutos en su viaje 



# 6. curva de potencia del test sobre el punto 5
data = recorrido9a66 %>% filter(!is.na(duracion_recorrido_min)) %>% select(duracion_recorrido_min)
n = nrow(data)
var = 0.2*0.8
pstar <- seq(from =0 , to=0.2, by=0.01)
pcrit <- qnorm(0.05)*(sqrt(var/n))+0.2
beta <- 1-pnorm((pcrit-pstar)/sqrt(var/n))
potencia <- 1-beta
plot(pstar,potencia, ylim=c(0,1),type = "l", xlab= "p*", ylab= "potencia", lwd=2)
#a medida que nos acercamos al 20% es menos potente rechazar Ho
#cuando efectivamente es Falsa.



# 7. En mayo de 2018 se inauguró la estación Facultad de Derecho de la línea
# H de subtes

#filtramos solo los recorridos bajo nuestro análisis
estacionFdD = recorridos %>% filter(id_estacion_origen==1 | id_estacion_destino==1) %>% 
  select(id_estacion_origen,fecha_origen_recorrido,id_estacion_destino,fecha_destino_recorrido)

#cambio a formato fecha
estacionFdD$fecha_origen_recorrido <- format(as.POSIXct(estacionFdD$fecha_origen_recorrido, format="%Y-%m-%d %H:%M:%OS"),"%m")
estacionFdD$fecha_destino_recorrido <- format(as.POSIXct(estacionFdD$fecha_destino_recorrido, format="%Y-%m-%d %H:%M:%OS"),"%m")

attach(estacionFdD)

origen = estacionFdD %>% filter(id_estacion_origen==1) %>% select(estacion=id_estacion_origen, fecha=fecha_origen_recorrido)# %>% ungroup()
destino = estacionFdD %>% filter(id_estacion_destino==1) %>% select(estacion=id_estacion_destino, fecha=fecha_destino_recorrido)# %>% ungroup()

origen = group_by(origen,fecha)
destino = group_by(destino,fecha)

uso_estacion = rbind(origen,destino)

uso_estacion = summarise(uso_estacion,cant=n())
uso_estacion$fecha = as.numeric(uso_estacion$fecha)

head(uso_estacion)

#vemos un gráfico de como se comporta el uso de la estacion y la fecha de uso
#gráficamos una línea vertical el 5 por ser el mes donde se creo la estación
qplot(uso_estacion$fecha,uso_estacion$cant) +
  geom_vline(xintercept = 5, color = "red") +
  geom_hline(yintercept = mean(uso_estacion$cant), color = "red")
#vemos que gráficamente podría decirse que el uso aumentó

#creamos variables de enero a abril y de junio a septiembre para comparar
#y hacer el test de hipótesis
cuatro_meses_pre = uso_estacion %>% filter(fecha>=1 & fecha<=4) %>% select(cuat1=cant)
cuatro_meses_pos = uso_estacion %>% filter(fecha>=6 & fecha<=9) %>% select(cuat2=cant)

#planteamos el test de hipotesis como diferencia de medias
# d = media cuatri 1 - media cuatri 2
# H0 <- d = 0 
# H1 <- d != 0

t.test(cuatro_meses_pos$cuat2,cuatro_meses_pre$cuat1,paired = TRUE, alternative = "greater")
# como p-value < a alfa entonces rechazamos H0
#existe suficiente evidencia estadística para rechazar H0


# 8. Probar si existen diferencias estadísticamente significativas en el uso del sistema de
#    EcoBici por género
recorridos_genero = recorridos %>% filter(genero_usuario %in% c("M","F")) %>% select(id_usuario, genero_usuario)
recorridos_genero$genero = factor(ifelse( recorridos_genero$genero_usuario=="M", 1 , 0))

#vemos el uso por género en una tabla
table(recorridos_genero$genero)

usuarios_hombres = recorridos_genero %>% select(genero) %>% filter(genero==1)
usuarios_hombres = nrow(usuarios_hombres)

usuarios_mujeres = recorridos_genero %>% select(genero) %>% filter(genero==0)
usuarios_mujeres = nrow(usuarios_mujeres)

n = length(recorridos_genero$genero)

#hacemos el test de hipotesis correspondiente
#H0: propM = propH
#H1: propM != propH

prop.test(x = usuarios_mujeres, n = n)
prop.test(x = usuarios_hombres, n = n)
binom.test(x = usuarios_hombres, n = n, p = .85, alternative = "two.sided")

# como p-value < a alfa entonces rechazamos H0
#existe suficiente evidencia estadística para rechazar H0
#las proporciones por género son diferentes.

# 9. Probar lo mismo que en el punto anterior, pero por grupo etario
recorridos_genero$edad = ifelse(recorridos_genero$id_usuario %in% usuarios$usuario_id, usuarios$usuario_edad, 0)

data = as.data.frame(table(recorridos_genero$edad[recorridos_genero$edad>8 & recorridos_genero$edad<85]))

qplot(data$Var1,data$Freq) +
  geom_vline(xintercept = data$Var1[data$Freq==max(data$Freq)], color = "red") +
  geom_vline(xintercept = data$Var1[data$Var1==43], color = "red") +
  geom_hline(yintercept = mean(data$Freq), color = "red")

#DIVIDIR POBLACION EN GRUPOS ETARIOS (15 EN 15)
#HACER T TEST DE CADA GRUPO
#VER QUE NO SE SOLAPEN


# 10. Bootstrap con punto 5
recorrido9a66$edad <- ifelse(recorrido9a66$id_usuario %in% usuarios$usuario_id, usuarios$usuario_edad, 0)
recorrido9a66 = recorrido9a66 %>% filter(!is.na(duracion_mayor_15min) & edad!=0)

#construyo la función asociada al estadístico de interés:
fc_cor = function(d,i){
  d = d[i,]
  return(cor(d$duracion_recorrido_min,d$edad))
}

fc_cor(recorrido9a66)

#usamos el comando boot:
set.seed(19042020) #si lo suprimo no puedo replicar los mismos resultados (variarían por la simulación)
boot_cor <- boot(data = recorrido9a66, statistic = fc_cor, R = 10000)
boot_cor
boot_cor$t #para ver las 1000 replicaciones
sd(boot_cor$t) #el error estándar
mean(boot_cor$t)-boot_cor$t0 #el sesgo
#histograma y qqplot
plot(boot_cor)
#intervalo de confianza:
#boot.ci(boot.out = boot_cor, type = c("norm", "basic", "perc", "bca"))
boot.ci(boot.out = boot_cor, type = "perc")



