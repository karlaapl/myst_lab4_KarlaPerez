rm(list = ls())

# los 0s aceptados antes de expresas una cifra en notaci?n cient?fica
options("scipen"=100, "digits"=4)
mdir <- getwd()
suppressMessages(library(TTR)) 
# -- Establecer el sistema de medicion de la computadora
Sys.setlocale(category = "LC_ALL", locale = "")

###librerias
suppressMessages(library(plotly))
suppressMessages(library(xlsx))# 
suppressMessages(library(Quandl)) # Descarga de Precios
suppressMessages(library(PortfolioAnalytics)) ##Teoria modenar de portafolio
suppressMessages(library(ROI)) # optimización para ele portafolio 
suppressMessages(library(knitr))  
suppressMessages(library(kableExtra)) # Tablas en HTML
options(knitr.table.format = "html") 

Historicos<-(read.csv(file="C:/Users/perezka/OneDrive - HP/Karla/Karla/Trading/myst_lab4_KarlaPerez/History.csv",header=TRUE))

Sys.setenv(tz="America/Monterrey", TZ="America/Monterrey")
options(tz="America/Monterrey", TZ="America/Monterrey")

# -- Cargar y/o instalar en automatico paquetes a utilizar -- #

pkg <- c("base","downloader","dplyr","fBasics","forecast","grid",
         "gridExtra","httr","jsonlite","lmtest","lubridate","moments",
         "matrixStats", "PerformanceAnalytics","plyr","quantmod",
         "reshape2","RCurl","RMySQL", "stats","scales","tseries",
         "TTR","TSA","XML","xts","zoo")

inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
instpackages <- lapply(pkg, library, character.only=TRUE)

# -- Cargar archivos desde GitHub -- #

RawGitHub <- "https://raw.githubusercontent.com/IFFranciscoME/"
ROandaAPI <- paste(RawGitHub,"ROandaAPI/master/ROandaAPI.R",sep="")
downloader::source_url(ROandaAPI,prompt=FALSE,quiet=TRUE)

# -- Parametros para usar API-OANDA

# Tipo de cuenta practice/live
OA_At <- "practice"
# ID de cuenta
OA_Ai <- 1742531
# Token para llamadas a API
OA_Ak <- "ada4a61b0d5bc0e5939365e01450b614-4121f84f01ad78942c46fc3ac777baa6" 
# Hora a la que se considera "Fin del dia"
OA_Da <- 16
# Uso horario
OA_Ta <- "GMT"
# Instrumento
OA_In <- "USD_MXN"
# Granularidad o periodicidad de los precios H4 = Cada 4 horas
OA_Pr <- "M1"
# Multiplicador de precios para convertir a PIPS
MultPIP_MT1 <- 10000


Historicos$Date<-as.character(Historicos$Date)

Historicos[is.na(Historicos)] <- 0
Historicos<- data.frame("Date" = Historicos$Date,
                        "Actual" = Historicos$Actual,
                        "Consensus"= Historicos$Consensus,
                        "Previous" = Historicos$Previous,
                        "Clasificacion" = NA )

#$Escenario.A<-which(Historicos$Actual>=Historicos$Consensus & Historicos$Consensus>=Historicos$Previous)

for(j in 1:length(Historicos$Date)){#for rows
  if (Historicos$Actual[j]>=Historicos$Consensus[j]&Historicos$Consensus[j]>=Historicos$Previous[j]) {
    Historicos$Clasificacion[j] <-"A"
  }#if A"
  else if (Historicos$Actual[j]>=Historicos$Consensus[j]&Historicos$Consensus[j]<Historicos$Previous[j]){
    Historicos$Clasificacion[j] <-"B"
  }#if b
  else if (Historicos$Actual[j]<Historicos$Consensus[j]&Historicos$Consensus[j]>=Historicos$Previous[j]){
    Historicos$Clasificacion[j] <-"C"
  }# c
  else {Historicos$Clasificacion[j] <-"D"}
  
  
}# fin 



####Codigo para pedir los precios a OAnda fecha año-mes-dia
# tomar cada fecha y descargar las fechaas
data <- list()

  
Historicos$Date <- as.character(as.POSIXct(Historicos$Date,format = "%m/%d/%Y %H:%M")) # conviert todas las fechas a fromato Oanda

for ( i in 1:38){# forzamos a que sean los 3 años 

fecha_ejm <- Historicos$Date[[i]]
aux <- Historicos$Clasificacion[[i]]
  

#opcion convertir a a fecha

F2 <- as.Date(substr(fecha_ejm,1,10))

  
if(wday(F2) != 1) # if para saber en que dia se encuentra
  #wday 
  #si da 1, domingo
{
 
  Fecha1 <- F2
  Fecha2 <- F2+1
  Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                             DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                             Instrument = OA_In, 
                             Start = Fecha1, End = Fecha2, Count = NULL)
} else {
  
  Fecha1 <- F2-2
  Fecha2 <- F2+1
  Precios_Oanda <- HisPrices(AccountType = OA_At, Granularity = OA_Pr,
                             DayAlign = OA_Da, TimeAlign = OA_Ta, Token = OA_Ak,
                             Instrument = OA_In, 
                             Start = Fecha1, End = Fecha2, Count = NULL)
}

Precios_Oanda$TimeStamp <- as.character(as.POSIXct(Precios_Oanda$TimeStamp,format = "%m/%d/%Y %H:%M:%S"))

#convierte de nuevo la fecha del xlsx para poder encontrarla en el DataFrame

ind <- which(Precios_Oanda$TimeStamp == Historicos$Date[[i]])



data[[i]] <- list("Escenario" = Historicos$Clasificacion[i],"Precios" = Precios_Oanda[(ind-15):(ind+15),])
#print(paste0("Iteracion: ",i, " Fecha 1 es: ", Fecha1, " Fecha 2 es: ", Fecha2))

Calculos$Rends[[i]]<-data[,list(mean = mean(data[[i]]$Precios$Close)), by = data$Escenario]

Calculos$Desv_s[[i]]<-data[,list(desv = stdev(data[[i]]$Precios$Close)), by = data$Escenario]
Calculos$Dif[[i]]<-data[,list(dif= data$Precios$Close[[1]] - data$Precios$Close[[31]]), by = data$Escenario]
Calculos$max[[i]]<-date[,list(max = max(data$Precios$Close)-min(data$Precios$Close)),by = data$Escenarios] 


}


