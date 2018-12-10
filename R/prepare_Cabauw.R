#'@title Prepare Cabauw data for UHImax calculations
#'@description The functions reads the 10min and hourly data and changes it to a `data.frame`. Using the
#'functions \code{\link{calc_U}},\code{\link{calc_S}} and \code{\link{calc_DTR}} the meteorological parameters
#'are prepared for the formula \code{\link{UHImax}}. From the hourly data a subsection of days is made which
#'are ideal for the UHImax relation (see \code{\link{uhi_sub}}).
#'@param ten_min full path of the 10 minute datafile downloaded from the local KIS server.
#'@param hourly_data full path of hourly data, also downloaded from the local KIS server.
#'@export
prepare_Cabauw<-function(ten_min="inst/Cabauw_meteo/cabauw10min.csv",
                         hourly_data="inst/Cabauw_meteo/cabauw_hourly.csv"){
  Cp<-1005 #specific heat capacity
  R=287.058 # specific gas constant for dry air (J/(kg·K))
  lambda=4
  message("Reading the 10min data")
  ten_min<-data.table::fread(ten_min)

  message("Preparing 10min variables")
  ten_min$IT_DATETIME<-as.POSIXct(ten_min$IT_DATETIME,format="%Y%m%d_%H%M00_000000")


  Cabauw.S<-subset(ten_min[which(ten_min$DS_CODE=="348_S_a"),],
                   select = c("IT_DATETIME","DS_CODE","TOS.Q_GLOB_10"))
  Cabauw.T<-subset(ten_min[which(ten_min$DS_CODE=="348_T_a"),],
                   select = c("IT_DATETIME","DS_CODE","TOT.T_DRYB_10"))
  Cabauw.W<-subset(ten_min[which(ten_min$DS_CODE=="348_W_a"),],
                   select = c("IT_DATETIME","DS_CODE","TOW.FF_SENSOR_10"))
  Cabauw.P<-subset(ten_min[which(ten_min$DS_CODE=="348_A_a")],
                   select = c("IT_DATETIME","DS_CODE","TOA.P_NAP_MSL_10"))

  Cabauw_10min<-data.frame(Cabauw.T,Cabauw.S$TOS.Q_GLOB_10,Cabauw.W$TOW.FF_SENSOR_10,Cabauw.P$TOA.P_NAP_MSL_10)
  names(Cabauw_10min)<-c("time","DS_CODE","T","S","U","P")

  #Calculate the meteo parameters for Theeuwes(2017)
  message("Calculating meteo parameters for Theeuwes(2017) using the 10min data")
  S<-calc_S(Cabauw.S$IT_DATETIME,Cabauw.S$TOS.Q_GLOB_10)
  DTR<-calc_DTR(Cabauw.T$IT_DATETIME,Cabauw.T$TOT.T_DRYB_10)
  U<-calc_U(Cabauw.W$IT_DATETIME,Cabauw.W$TOW.FF_SENSOR_10)

  air_density<-calc_U(Cabauw_10min$time,(Cabauw_10min$P*100)/(R*(Cabauw_10min$T+273.15)))
  names(air_density)<-c("start","stop","rho")

  Meteo_params<-data.frame(DTR,"S"=S$S[1:length(DTR$start)],"U"=U$W,"rho"=air_density$rho)
  Meteo_params$start<-as.POSIXct(Meteo_params$start)
  Meteo_params$stop<-as.POSIXct(Meteo_params$stop)

  Meteo_params$S_new<-Meteo_params$S/(Meteo_params$rho*Cp)
  Meteo_params$meteo<-(((Meteo_params$S_new)*Meteo_params$DTR^3)/Meteo_params$U)^(1/lambda)


  #Format the hourly data
  message("Reading hourly data")
  hourly_data<-data.table::fread(hourly_data)
  hourly_data$IT_DATETIME<-as.POSIXct(hourly_data$IT_DATETIME,format="%Y%m%d_%H%M00_000000")
  time<-hourly_data$IT_DATETIME
  rh<-hourly_data$BGH.U
  rain<-hourly_data$BGH.Q_RH
  wind<-hourly_data$BGH.FH

  Cabauw_hourly<-data.frame(time,rh,rain,wind)
  names(Cabauw_hourly)<-c("time","RH","rain","U")

  #Run the uhi_sub function on the hourly data
  days_subset<-uhi_sub("time"=time,"wind"=wind,"rain"=rain,"rh"=rh)



  return(list("Cabauw_10min" = Cabauw_10min,
              "Cabauw_hour"= Cabauw_hourly,
              "Cabauw_Theeuwes" = Meteo_params,
              "Cabauw_sub" = days_subset))
}
